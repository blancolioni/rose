with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

with Rose.Limits;

with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Objects;
with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Storage.Dependent.Client;
with Rose.Interfaces.Storage.Region.Client;

with Exec.Output;

package body Exec.Library is

   use System.Storage_Elements;

   Max_Start_Caps         : constant := 16;

   subtype Installed_Program_Name is String (1 .. Max_Name_Length);

   type Installed_Program_Record is
      record
         Caps            : Rose.Capabilities.Capability_Array
                             (1 .. Max_Start_Caps);
         Name            : Installed_Program_Name;
         ELF_Base        : Rose.Objects.Page_Object_Id;
         ELF_Bound       : Rose.Objects.Page_Object_Id;
         Image           : Rose.Capabilities.Capability :=
                             Rose.Capabilities.Null_Capability;
         --  A region of the image's pages, which is exec's.
         Image_Access    : Rose.Capabilities.Capability :=
                             Rose.Capabilities.Null_Capability;
         --  Its Region_Access, which elf is given, and can't release
         --  (#403).
         Metadata_Offset : Storage_Count := 0;
         Metadata_Length : Storage_Count := 0;
         Retired         : Boolean := False;
         --  A newer version replaced it (Update). Its pages are kept
         --  while processes still run it, because they map them.
         Successor       : Rose.Objects.Capability_Identifier := 0;
         --  The version that replaced it, if it is retired. Its launch
         --  caps launch the newest version (Newest, #902).
         Pinned          : Boolean := False;
         --  A process of it couldn't be registered with its bank, so
         --  exec can't tell when it ends: the pages are never freed.
         Freed           : Boolean := False;
         --  It was retired, and no process runs it: its pages have been
         --  given back, and may hold another program's image (#875).
      end record;

   package Program_Vectors is
     new Ada.Containers.Vectors (Positive, Installed_Program_Record);

   Installed_Programs : Program_Vectors.Vector;
   --  The programs, in the order they were installed: program Id is
   --  element Id. There is no limit on their number (#490), apart from
   --  the room for their images in the region.

   function Is_Installed
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean
   is (Id in 1 .. Rose.Objects.Capability_Identifier
                   (Installed_Programs.Length));

   function Program
     (Id : Rose.Objects.Capability_Identifier)
      return Program_Vectors.Reference_Type
   is (Installed_Programs.Reference (Positive (Id)));
   --  Program Id, which Is_Installed.

   Region         : Rose.Interfaces.Storage.Region.Client.Region_Client;
   --  Where the images are kept.
   Page_Base,
   Page_Bound         : Rose.Objects.Object_Id := 0;
   Next_Page          : Rose.Objects.Object_Id := 0;
   --  The region's pages from Next_Page on have never held an image.

   type Page_Range is
      record
         Base, Bound : Rose.Objects.Object_Id;
      end record;

   package Range_Vectors is
     new Ada.Containers.Vectors (Positive, Page_Range);

   Free_Ranges : Range_Vectors.Vector;
   --  The ranges below Next_Page that freed images held (#875), in
   --  order, none empty, and none touching another or Next_Page.

   type Bank_Access is
     access Rose.Interfaces.Storage.Bank.Client.Bank_Client;
   type Dependent_Access is
     access Rose.Interfaces.Storage.Dependent.Client.Dependent_Client;
   --  The clients are limited, so a vector holds them by reference.

   procedure Free_Bank is
     new Ada.Unchecked_Deallocation
       (Rose.Interfaces.Storage.Bank.Client.Bank_Client, Bank_Access);
   procedure Free_Dependent is
     new Ada.Unchecked_Deallocation
       (Rose.Interfaces.Storage.Dependent.Client.Dependent_Client,
        Dependent_Access);

   type Process_Record is
      record
         Program   : Rose.Objects.Capability_Identifier;
         Bank      : Bank_Access;
         --  The process's own bank, which is destroyed when it ends.
         Dependent : Dependent_Access;
         --  Its registration with Bank, or null if there is none.
      end record;

   package Process_Vectors is
     new Ada.Containers.Vectors (Positive, Process_Record);

   Processes  : Process_Vectors.Vector;
   --  The processes that exec has launched, and that haven't ended.

   Signal_Cap : Rose.Capabilities.Capability :=
                  Rose.Capabilities.Null_Capability;

   function Register (Process : in out Process_Record) return Boolean;
   --  Register Process with its bank. False if the store refuses,
   --  because the bank has been destroyed, or it has no room.

   function Bank_In_Use (Process : in out Process_Record) return Boolean;
   --  True if Process's bank still has something reserved from it. A
   --  process's own bank always has, its stack at least, until it is
   --  destroyed; the store reports nothing for a bank that has gone.
   --  This tells a destroyed bank from a store that has no room for
   --  another registration.

   function Has_Ended (Process : in out Process_Record) return Boolean;
   --  True if Process's bank has been destroyed.

   procedure Forget (Index : Positive);
   --  Forget process Index, which has ended: end its registration, and
   --  delete its caps.

   procedure Forget_Registration (Process : in out Process_Record);
   --  Delete the caps of Process's registration, without ending it.

   procedure Close_Bank (Process : in out Process_Record);
   --  Delete the caps of Process's bank.

   procedure Free_Unused;
   --  Free each retired version that no process runs, and that isn't
   --  pinned.

   procedure Free (Id : Rose.Objects.Capability_Identifier);
   --  Free program Id's image.

   procedure Add_Free_Range (Base, Bound : Rose.Objects.Object_Id);
   --  The pages from Base to Bound are free to hold an image again.

   procedure Pin (Id : Rose.Objects.Capability_Identifier);
   --  A process of program Id can't be registered, so its image is
   --  never freed.

   function Image (X : Natural) return String;
   --  X, with no leading space.

   function Program_Name
     (Id : Rose.Objects.Capability_Identifier)
      return String;
   --  The name program Id was installed with.

   subtype Page_Buffer is Storage_Array (1 .. Rose.Limits.Page_Size);

   type Cached_Page is
      record
         Valid : Boolean := False;
         Page  : Rose.Objects.Object_Id := 0;
         Data  : Page_Buffer;
      end record;

   Cache      : array (1 .. 2) of Cached_Page;
   Next_Entry : Positive := 1;
   --  The pages of images that Read_Image read last. Reading section
   --  headers goes back and forth between the headers and their names.

   procedure Invalidate_Cache;
   --  Forget the cached pages: their images may have been freed, and
   --  their pages written with another's (#875).

   Zero_Page : constant Page_Buffer := (others => 0);

   procedure Zero_Pages (Base, Bound : Rose.Objects.Object_Id);
   --  Write zeroes to the pages from Base to Bound, which frees their
   --  space in the store (#945).

   function Padded (Name : String) return Installed_Program_Name;
   --  Name as a program record holds it: cut, or padded with spaces.

   procedure Write_Image
     (ELF_Image : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array;
      Old       : Natural;
      Item      : out Installed_Program_Record;
      Same      : out Boolean;
      Success   : out Boolean);
   --  Copy ELF_Image into the region's free pages, and make Item the
   --  program record for it, which is not added yet. Same is True if Old
   --  is a program whose image is the same, page for page, and neither
   --  has install caps: then nothing is written (#1109). Success is
   --  False, and nothing is kept, if the image doesn't fit.

   procedure Find_Metadata (Program : in out Installed_Program_Record);
   --  Find the .rose_command section of Program's ELF image, if it has
   --  one.

   procedure Read_Image
     (Program : Installed_Program_Record;
      Offset  : Storage_Count;
      Data    : out Storage_Array;
      Valid   : out Boolean);
   --  Data from Program's ELF image, starting at Offset. Valid is False
   --  if that is not all in the image.

   function Unsigned
     (Data   : Storage_Array;
      Offset : Storage_Offset;
      Size   : Positive)
      return Storage_Count;
   --  The little-endian integer of Size bytes at Offset in Data, counting
   --  from zero.

   --------------------
   -- Add_Free_Range --
   --------------------

   procedure Add_Free_Range (Base, Bound : Rose.Objects.Object_Id) is
      use type Rose.Objects.Object_Id;
      New_Range : Page_Range := (Base, Bound);
      Index     : Positive := 1;
   begin

      --  Find where it goes, after the ranges that end before it, and
      --  join it to the ranges on either side that it touches.

      while Index <= Free_Ranges.Last_Index
        and then Free_Ranges (Index).Bound < Base
      loop
         Index := Index + 1;
      end loop;

      if Index <= Free_Ranges.Last_Index
        and then Free_Ranges (Index).Bound = Base
      then
         New_Range.Base := Free_Ranges (Index).Base;
         Free_Ranges.Delete (Index);
      end if;

      if Index <= Free_Ranges.Last_Index
        and then Free_Ranges (Index).Base = Bound
      then
         New_Range.Bound := Free_Ranges (Index).Bound;
         Free_Ranges.Delete (Index);
      end if;

      --  A range that reaches Next_Page is part of the end again.

      if New_Range.Bound = Next_Page then
         Next_Page := New_Range.Base;
      else
         Free_Ranges.Insert (Index, New_Range);
      end if;
   end Add_Free_Range;

   -----------------
   -- Add_Process --
   -----------------

   procedure Add_Process
     (Id   : Rose.Objects.Capability_Identifier;
      Bank : Rose.Capabilities.Capability)
   is
      use type Rose.Capabilities.Capability;
      Process : Process_Record;
   begin
      if Bank = Rose.Capabilities.Null_Capability then
         if Is_Installed (Id) then
            Pin (Id);
         end if;
         return;
      end if;

      if not Is_Installed (Id) then
         Rose.System_Calls.Delete_Cap (Bank);
         return;
      end if;

      Process.Program := Id;
      Process.Bank :=
        new Rose.Interfaces.Storage.Bank.Client.Bank_Client'
          (Rose.Interfaces.Storage.Bank.Client.From_Cap (Bank));

      --  The process hasn't started, so its bank is there, and a failed
      --  registration means a full store: the program is pinned.

      if Register (Process) then
         Processes.Append (Process);
      else
         Close_Bank (Process);
         Pin (Id);
      end if;
   end Add_Process;

   -----------------
   -- Bank_In_Use --
   -----------------

   function Bank_In_Use (Process : in out Process_Record) return Boolean is
      use type Rose.Words.Word;
      use type Rose.Words.Word_64;
      Pages        : Rose.Words.Word_64;
      Regions      : Rose.Words.Word;
      Page_Limit   : Rose.Words.Word_64;
      Region_Limit : Rose.Words.Word;
   begin
      Process.Bank.Get_Usage (Pages, Regions, Page_Limit, Region_Limit);
      return Pages /= 0 or else Regions /= 0;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return False;
   end Bank_In_Use;

   ---------------------
   -- Check_Processes --
   ---------------------

   procedure Check_Processes is
   begin
      for Index in reverse 1 .. Processes.Last_Index loop
         if Has_Ended (Processes (Index)) then
            Forget (Index);
         end if;
      end loop;
      Free_Unused;
   end Check_Processes;

   ----------------
   -- Close_Bank --
   ----------------

   procedure Close_Bank (Process : in out Process_Record) is
   begin
      if Process.Bank /= null then
         Rose.Interfaces.Storage.Bank.Client.Close (Process.Bank.all);
         Free_Bank (Process.Bank);
      end if;
   end Close_Bank;

   -------------------
   -- Find_Metadata --
   -------------------

   procedure Find_Metadata (Program : in out Installed_Program_Record) is
      Section_Name : constant String := ".rose_command";
      Header       : Storage_Array (0 .. 51);
      Valid        : Boolean;
   begin
      Read_Image (Program, 0, Header, Valid);
      if not Valid
        or else Header (0 .. 3) /=
          (16#7F#, Character'Pos ('E'), Character'Pos ('L'),
           Character'Pos ('F'))
      then
         return;
      end if;

      declare
         Table_Offset : constant Storage_Count := Unsigned (Header, 32, 4);
         Entry_Size   : constant Storage_Count := Unsigned (Header, 46, 2);
         Entry_Count  : constant Storage_Count := Unsigned (Header, 48, 2);
         Names_Index  : constant Storage_Count := Unsigned (Header, 50, 2);
         Section      : Storage_Array (0 .. 39);
         Names_Offset : Storage_Count;
      begin
         if Entry_Size < Section'Length or else Names_Index >= Entry_Count
         then
            return;
         end if;

         Read_Image
           (Program, Table_Offset + Names_Index * Entry_Size,
            Section, Valid);
         if not Valid then
            return;
         end if;
         Names_Offset := Unsigned (Section, 16, 4);

         for Index in 0 .. Entry_Count - 1 loop
            Read_Image
              (Program, Table_Offset + Index * Entry_Size, Section, Valid);
            exit when not Valid;

            declare
               Name : Storage_Array (1 .. Section_Name'Length + 1);
            begin
               Read_Image
                 (Program, Names_Offset + Unsigned (Section, 0, 4),
                  Name, Valid);
               if Valid and then Name (Name'Last) = 0 then
                  declare
                     Found : Boolean := True;
                  begin
                     for I in Section_Name'Range loop
                        if Name (Storage_Offset (I))
                          /= Character'Pos (Section_Name (I))
                        then
                           Found := False;
                           exit;
                        end if;
                     end loop;

                     if Found then
                        Program.Metadata_Offset := Unsigned (Section, 16, 4);
                        Program.Metadata_Length := Unsigned (Section, 20, 4);
                        return;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end;
   end Find_Metadata;

   ------------
   -- Forget --
   ------------

   procedure Forget (Index : Positive) is
   begin
      declare
         Process : Process_Record renames Processes (Index);
      begin

         --  Release ends the registration, which the store keeps until
         --  then; it fails if the store no longer knows it.

         if Process.Dependent /= null then
            begin
               Process.Dependent.Release;
            exception
               when Rose.Interfaces.Objects.Invocation_Error =>
                  null;
            end;
         end if;
         Forget_Registration (Process);
         Close_Bank (Process);
      end;
      Processes.Delete (Index);
   end Forget;

   -------------------------
   -- Forget_Registration --
   -------------------------

   procedure Forget_Registration (Process : in out Process_Record) is
   begin
      if Process.Dependent /= null then
         Rose.Interfaces.Storage.Dependent.Client.Forget
           (Process.Dependent.all);
         Free_Dependent (Process.Dependent);
      end if;
   end Forget_Registration;

   ----------
   -- Free --
   ----------

   procedure Free (Id : Rose.Objects.Capability_Identifier) is
      use type Rose.Capabilities.Capability;
      use type Rose.Objects.Object_Id;
      Item : Installed_Program_Record renames Program (Id);
   begin

      --  Elf has closed its Region_Access, and mem has dropped the
      --  segments of the processes that mapped the pages, so the
      --  subregion is exec's alone.

      if Item.Image /= Rose.Capabilities.Null_Capability then
         declare
            Subregion : Rose.Interfaces.Storage.Region.Client.Region_Client
              := Rose.Interfaces.Storage.Region.Client.From_Cap (Item.Image);
         begin
            Rose.Interfaces.Storage.Region.Client.Close (Subregion);
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               Rose.Interfaces.Storage.Region.Client.Forget (Subregion);
         end;
         Rose.System_Calls.Delete_Cap (Item.Image_Access);
         Item.Image := Rose.Capabilities.Null_Capability;
         Item.Image_Access := Rose.Capabilities.Null_Capability;
      end if;

      Zero_Pages (Item.ELF_Base, Item.ELF_Bound);
      Add_Free_Range (Item.ELF_Base, Item.ELF_Bound);
      Item.Freed := True;

      --  The pages in use, for the update test (#875): the range up to
      --  Next_Page, less the free ranges below it.

      declare
         In_Use : Rose.Objects.Object_Id := Next_Page - Page_Base;
      begin
         for Free_Range of Free_Ranges loop
            In_Use := In_Use - (Free_Range.Bound - Free_Range.Base);
         end loop;

         Exec.Output.Put_Line
           ("exec: freed "
            & Image (Natural (Item.ELF_Bound - Item.ELF_Base))
            & " pages of " & Program_Name (Id)
            & "; " & Image (Natural (In_Use)) & " in use");
      end;
   end Free;

   -----------------
   -- Free_Unused --
   -----------------

   procedure Free_Unused is
      use type Rose.Objects.Capability_Identifier;
   begin
      for Index in 1 .. Installed_Programs.Last_Index loop
         declare
            Id      : constant Rose.Objects.Capability_Identifier :=
              Rose.Objects.Capability_Identifier (Index);
            Running : Boolean := False;
         begin
            if Program (Id).Retired
              and then not Program (Id).Pinned
              and then not Program (Id).Freed
            then
               for Process of Processes loop
                  if Process.Program = Id then
                     Running := True;
                     exit;
                  end if;
               end loop;

               if not Running then
                  Free (Id);
               end if;
            end if;
         end;
      end loop;
   end Free_Unused;

   ------------------
   -- Get_Metadata --
   ------------------

   procedure Get_Metadata
     (Id     : Rose.Objects.Capability_Identifier;
      Result : out String;
      Last   : out Natural)
   is
   begin
      Last := Result'First - 1;

      if not Is_Installed (Id)
        or else Program (Id).Metadata_Length = 0
      then
         return;
      end if;

      declare
         Item   : Installed_Program_Record renames Program (Id);
         Length : constant Storage_Count :=
           Storage_Count'Min
             (Item.Metadata_Length, Storage_Count (Result'Length));
         Data   : Storage_Array (1 .. Length);
         Valid  : Boolean;
      begin
         Read_Image (Item, Item.Metadata_Offset, Data, Valid);
         if Valid then
            for Byte of Data loop
               Last := Last + 1;
               Result (Last) := Character'Val (Byte);
            end loop;
         end if;
      end;
   end Get_Metadata;

   ----------------------
   -- Get_Program_Name --
   ----------------------

   procedure Get_Program_Name
     (Id   : Rose.Objects.Capability_Identifier;
      Name : out String;
      Last : out Natural)
   is
   begin
      Last := Name'First - 1;
      if not Is_Installed (Id) then
         return;
      end if;

      for Ch of Program (Id).Name loop
         exit when Ch = ' ' or else Last = Name'Last;
         Last := Last + 1;
         Name (Last) := Ch;
      end loop;
   end Get_Program_Name;

   ---------------
   -- Has_Ended --
   ---------------

   --  If the store doesn't know the registration, the bank decides.

   function Has_Ended (Process : in out Process_Record) return Boolean is
   begin
      return (if Process.Dependent = null
              then not Bank_In_Use (Process)
              else Process.Dependent.Destroyed);
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return not Bank_In_Use (Process);
   end Has_Ended;

   -----------
   -- Image --
   -----------

   function Image (X : Natural) return String is
      Result : constant String := Natural'Image (X);
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Image;

   ------------------
   -- Image_Region --
   ------------------

   function Image_Region
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      Item : Installed_Program_Record renames Program (Id);
   begin
      if Item.Image = Rose.Capabilities.Null_Capability then
         declare
            Image : Rose.Interfaces.Storage.Region.Region_Object'Class :=
              Region.Create_Subregion (Item.ELF_Base, Item.ELF_Bound, 5);
         begin
            Item.Image := Image.Region_Cap;
            Item.Image_Access := Image.Region_Access_Cap;
         end;
      end if;
      return Item.Image_Access;
   end Image_Region;

   -------------
   -- Install --
   -------------

   function Install
     (ELF_Image : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array)
      return Rose.Objects.Capability_Identifier
   is
      Item    : Installed_Program_Record;
      Same    : Boolean;
      Success : Boolean;
   begin
      Write_Image (ELF_Image, Name, Caps, 0, Item, Same, Success);
      if not Success then
         return 0;
      end if;

      Find_Metadata (Item);
      Installed_Programs.Append (Item);
      return Rose.Objects.Capability_Identifier
        (Installed_Programs.Last_Index);
   end Install;

   ------------------
   -- Install_Caps --
   ------------------

   function Install_Caps
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability_Array
   is
      use type Rose.Capabilities.Capability;
   begin
      if Is_Installed (Id) then
         for I in Program (Id).Caps'Range loop
            if Program (Id).Caps (I) = 0 then
               return Program (Id).Caps (1 .. I - 1);
            end if;
         end loop;
         return Program (Id).Caps;
      end if;
      return (1 .. 0 => 0);
   end Install_Caps;

   ----------------------
   -- Invalidate_Cache --
   ----------------------

   procedure Invalidate_Cache is
   begin
      for Item of Cache loop
         Item.Valid := False;
      end loop;
   end Invalidate_Cache;

   ------------
   -- Newest --
   ------------

   function Newest
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Capability_Identifier
   is
      use type Rose.Objects.Capability_Identifier;
      Current : Rose.Objects.Capability_Identifier := Id;
   begin
      if not Is_Installed (Current) then
         return 0;
      end if;

      while Program (Current).Successor /= 0 loop
         Current := Program (Current).Successor;
      end loop;

      return Current;
   end Newest;

   ------------
   -- Padded --
   ------------

   function Padded (Name : String) return Installed_Program_Name is
      Result : Installed_Program_Name := (others => ' ');
   begin
      if Name'Length <= Max_Name_Length then
         Result (1 .. Name'Length) := Name;
      else
         Result := Name (Name'First .. Name'First + Max_Name_Length - 1);
      end if;
      return Result;
   end Padded;

   ---------
   -- Pin --
   ---------

   procedure Pin (Id : Rose.Objects.Capability_Identifier) is
   begin
      if not Program (Id).Pinned then
         Program (Id).Pinned := True;
         Exec.Output.Put_Line
           ("exec: can't follow a process of " & Program_Name (Id)
            & ", so its image is kept");
      end if;
   end Pin;

   ------------------
   -- Program_Name --
   ------------------

   function Program_Name
     (Id : Rose.Objects.Capability_Identifier)
      return String
   is
      Name : String (1 .. Max_Name_Length);
      Last : Natural;
   begin
      Get_Program_Name (Id, Name, Last);
      return Name (1 .. Last);
   end Program_Name;

   ----------------
   -- Read_Image --
   ----------------

   procedure Read_Image
     (Program : Installed_Program_Record;
      Offset  : Storage_Count;
      Data    : out Storage_Array;
      Valid   : out Boolean)
   is
      use type Rose.Objects.Object_Id;
      Page_Size  : constant Storage_Count := Rose.Limits.Page_Size;
      Image_Size : constant Storage_Count :=
        Storage_Count (Program.ELF_Bound - Program.ELF_Base) * Page_Size;
   begin
      Valid := not Program.Freed
        and then Offset <= Image_Size
        and then Data'Length <= Image_Size - Offset;
      if not Valid then
         return;
      end if;

      for Index in Data'Range loop
         declare
            Position : constant Storage_Count :=
              Offset + Storage_Count (Index - Data'First);
            Page     : constant Rose.Objects.Object_Id :=
              Program.ELF_Base + Rose.Objects.Object_Id (Position / Page_Size);
            Cached   : Natural := 0;
         begin
            for I in Cache'Range loop
               if Cache (I).Valid and then Cache (I).Page = Page then
                  Cached := I;
               end if;
            end loop;

            if Cached = 0 then
               Cached := Next_Entry;
               Next_Entry := (if Next_Entry = Cache'Last then Cache'First
                              else Next_Entry + 1);
               Region.Get (Page, Cache (Cached).Data);
               Cache (Cached).Valid := True;
               Cache (Cached).Page := Page;
            end if;

            Data (Index) := Cache (Cached).Data (Position mod Page_Size + 1);
         end;
      end loop;
   end Read_Image;

   --------------
   -- Register --
   --------------

   function Register (Process : in out Process_Record) return Boolean is
      use type Rose.Capabilities.Capability;
   begin
      declare
         Dependent : Rose.Interfaces.Storage.Dependent.Dependent_Object'Class
           := Process.Bank.Add_Dependent (Signal_Cap);
         Cap       : constant Rose.Capabilities.Capability :=
           Dependent.Dependent_Cap;
      begin
         if Cap = Rose.Capabilities.Null_Capability then
            return False;
         end if;
         Process.Dependent :=
           new Rose.Interfaces.Storage.Dependent.Client.Dependent_Client'
             (Rose.Interfaces.Storage.Dependent.Client.From_Cap (Cap));
      end;
      return True;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return False;
   end Register;

   --------------------
   -- Register_Again --
   --------------------

   procedure Register_Again is
   begin
      for Index in reverse 1 .. Processes.Last_Index loop
         declare
            Keep : Boolean := True;
         begin
            declare
               Process : Process_Record renames Processes (Index);
            begin

               --  The old registration went with the restore; only its
               --  caps are left.

               Forget_Registration (Process);

               --  A bank that is still in use, but can't be registered
               --  with, is a store with no room: its program is pinned.
               --  One with nothing in it has gone, and so has its
               --  process.

               if not Register (Process) then
                  if Bank_In_Use (Process) then
                     Pin (Process.Program);
                  end if;
                  Close_Bank (Process);
                  Keep := False;
               end if;
            end;

            if not Keep then
               Processes.Delete (Index);
            end if;
         end;
      end loop;
      Free_Unused;
   end Register_Again;

   --------------------
   -- Set_Region --
   --------------------

   procedure Set_Region (Region : Rose.Capabilities.Capability) is
   begin
      Rose.Interfaces.Storage.Region.Client.Open (Exec.Library.Region, Region);
      Exec.Library.Region.Get_Range (Page_Base, Page_Bound);
      Next_Page := Page_Base;
   end Set_Region;

   ----------------
   -- Set_Signal --
   ----------------

   procedure Set_Signal (Signal : Rose.Capabilities.Capability) is
   begin
      Signal_Cap := Signal;
   end Set_Signal;

   --------------
   -- Unsigned --
   --------------

   function Unsigned
     (Data   : Storage_Array;
      Offset : Storage_Offset;
      Size   : Positive)
      return Storage_Count
   is
      Result : Storage_Count := 0;
   begin
      for I in reverse 0 .. Storage_Offset (Size) - 1 loop
         Result := Result * 256
           + Storage_Count (Data (Data'First + Offset + I));
      end loop;
      return Result;
   end Unsigned;

   ------------
   -- Update --
   ------------

   function Update
     (ELF_Image : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array;
      Changed   : out Boolean)
      return Rose.Objects.Capability_Identifier
   is
      Old     : Natural := 0;
      Item    : Installed_Program_Record;
      Same    : Boolean;
      Success : Boolean;
   begin
      Changed := False;

      --  The newest version that isn't retired is the installed one.

      for Id in reverse 1 .. Installed_Programs.Last_Index loop
         if not Installed_Programs (Id).Retired
           and then Installed_Programs (Id).Name = Padded (Name)
         then
            Old := Id;
            exit;
         end if;
      end loop;

      Write_Image (ELF_Image, Name, Caps, Old, Item, Same, Success);
      if not Success then
         return 0;
      end if;

      if Same then

         --  Write_Image wrote nothing for it: the installed version
         --  stays.

         return Rose.Objects.Capability_Identifier (Old);
      end if;

      Find_Metadata (Item);
      Installed_Programs.Append (Item);
      Changed := True;

      if Old /= 0 then
         Installed_Programs (Old).Retired := True;
         Installed_Programs (Old).Successor :=
           Rose.Objects.Capability_Identifier
             (Installed_Programs.Last_Index);

         --  A command that isn't running can go now (#875).

         Free_Unused;
      end if;

      return Rose.Objects.Capability_Identifier
        (Installed_Programs.Last_Index);
   end Update;

   -----------------
   -- Write_Image --
   -----------------

   procedure Write_Image
     (ELF_Image : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array;
      Old       : Natural;
      Item      : out Installed_Program_Record;
      Same      : out Boolean;
      Success   : out Boolean)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type Rose.Objects.Object_Id;
      use type Rose.Capabilities.Capability;
      Buffer     : Page_Buffer;
      Data       : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length)
        with Import, Address => Buffer'Address;
      --  Buffer, as the image is read.
      Last       : Ada.Streams.Stream_Element_Offset;
      Old_Page   : Page_Buffer;
      Old_Base   : Rose.Objects.Object_Id := 0;
      Old_Bound  : Rose.Objects.Object_Id := 0;
      Page_Index : Rose.Objects.Object_Id := 0;
      --  The number of the image's pages read so far.
      Matching   : Boolean;
      --  Every page read so far is the same as Old's page at that index,
      --  and none has been written: the new image's first Page_Index
      --  pages are Old's, which can be copied from there if it differs.
      Cursor     : Rose.Objects.Object_Id;
      --  Where the image's next page is written.
      Limit      : Rose.Objects.Object_Id := Page_Bound;
      In_Free    : Natural := 0;
      --  The image is written in Free_Ranges (In_Free), which ends at
      --  Limit, or from Next_Page if In_Free is 0.

      procedure Not_Installed (Message : String);
      --  Report that the image is not installed, and give back the pages
      --  written for it: a page of zeroes takes no space in the store.

      function Put_Page (Page : Page_Buffer) return Boolean;
      --  Write Page as the image's next page. If it doesn't fit, or the
      --  store has no space for it, the image is not installed, and the
      --  result is False.

      function Move_To_End return Boolean;
      --  The image has outgrown the free range it was being written in:
      --  move the pages written so far to Next_Page, and go on from
      --  there. False, and the image is not installed, if they don't
      --  fit, or the store has no space for them.

      function Copy_Old_Pages (Count : Rose.Objects.Object_Id) return Boolean;
      --  Write Old's first Count pages as the image's first pages: the
      --  ones that matched before a page differed. False if they don't
      --  fit, as for Put_Page.

      --------------------
      -- Copy_Old_Pages --
      --------------------

      function Copy_Old_Pages
        (Count : Rose.Objects.Object_Id)
         return Boolean
      is
      begin
         for Index in 1 .. Count loop
            Region.Get (Old_Base + Index - 1, Old_Page);
            if not Put_Page (Old_Page) then
               return False;
            end if;
         end loop;
         return True;
      end Copy_Old_Pages;

      -----------------
      -- Move_To_End --
      -----------------

      function Move_To_End return Boolean is
         Count     : constant Rose.Objects.Object_Id :=
           Cursor - Item.ELF_Base;
         Move_Page : Page_Buffer;
      begin
         if Count > Page_Bound - Next_Page then
            Not_Installed ("no room for");
            return False;
         end if;

         for Index in 0 .. Count - 1 loop
            Region.Get (Item.ELF_Base + Index, Move_Page);
            begin
               Region.Put (Next_Page + Index, Move_Page);
            exception
               when Rose.Interfaces.Objects.Invocation_Error =>
                  Zero_Pages (Next_Page, Next_Page + Index);
                  Not_Installed ("no space for");
                  return False;
            end;
         end loop;

         Zero_Pages (Item.ELF_Base, Cursor);
         Item.ELF_Base := Next_Page;
         Cursor := Next_Page + Count;
         Limit := Page_Bound;
         In_Free := 0;
         return True;
      end Move_To_End;

      -------------------
      -- Not_Installed --
      -------------------

      --  Next_Page and Free_Ranges change only once the image is
      --  installed, so they need nothing back.

      procedure Not_Installed (Message : String) is
      begin
         Exec.Output.Put_Line ("exec: " & Message & " " & Name);
         Zero_Pages (Item.ELF_Base, Cursor);
         Same := False;
         Success := False;
      end Not_Installed;

      --------------
      -- Put_Page --
      --------------

      function Put_Page (Page : Page_Buffer) return Boolean is
      begin

         --  An image that has outgrown the free range it was put in
         --  moves to the end.

         if Cursor >= Limit and then In_Free /= 0
           and then not Move_To_End
         then
            return False;
         end if;

         --  An image that doesn't fit is not installed, rather than
         --  installed with its last pages missing (#669).

         if Cursor >= Limit then
            Not_Installed ("no room for");
            return False;
         end if;

         --  Nor is one that the store has no space for (#945).

         begin
            Region.Put (Cursor, Page);
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               Not_Installed ("no space for");
               return False;
         end;

         Cursor := Cursor + 1;
         return True;
      end Put_Page;

   begin
      Item.Caps := (others => 0);
      Item.Caps (1 .. Caps'Length) := Caps;
      Item.Name := Padded (Name);

      --  Install caps can't be compared, since a cap's number says
      --  nothing about what it is, so a program with any is changed.

      Matching := Old /= 0
        and then Caps'Length = 0
        and then Installed_Programs (Old).Caps (1) = 0;

      if Old /= 0 then
         Old_Base := Installed_Programs (Old).ELF_Base;
         Old_Bound := Installed_Programs (Old).ELF_Bound;
      end if;

      --  A new version goes in the first free range that its old
      --  version would fit (#875). If it turns out bigger than that, it
      --  moves to the end.

      Item.ELF_Base := Next_Page;
      if Old /= 0 then
         for Index in 1 .. Free_Ranges.Last_Index loop
            if Free_Ranges (Index).Bound - Free_Ranges (Index).Base
              >= Old_Bound - Old_Base
            then
               In_Free := Index;
               Item.ELF_Base := Free_Ranges (Index).Base;
               Limit := Free_Ranges (Index).Bound;
               exit;
            end if;
         end loop;
      end if;
      Cursor := Item.ELF_Base;

      --  While the image matches Old, nothing is written, so that an
      --  unchanged program needs no room and no store space (#1109).

      loop
         ELF_Image.Read (Data, Last);
         exit when Last < Data'First;

         Buffer (Storage_Offset (Last) + 1 .. Buffer'Last) :=
           (others => 0);

         if Matching then
            if Old_Base + Page_Index < Old_Bound then
               Region.Get (Old_Base + Page_Index, Old_Page);
               Matching := Old_Page = Buffer;
            else
               Matching := False;
            end if;

            if not Matching and then not Copy_Old_Pages (Page_Index) then
               return;
            end if;
         end if;

         if not Matching and then not Put_Page (Buffer) then
            return;
         end if;

         Page_Index := Page_Index + 1;
      end loop;

      Same := Matching and then Old_Base + Page_Index = Old_Bound;

      --  An image that is a shorter copy of the start of Old's is its
      --  own program, and its pages haven't been written yet.

      if Matching and then not Same
        and then not Copy_Old_Pages (Page_Index)
      then
         return;
      end if;

      --  The pages written are taken from the free range, or from the
      --  end. What Read_Image's cache holds of them was another image's.

      if In_Free /= 0 then
         if Cursor = Free_Ranges (In_Free).Bound then
            Free_Ranges.Delete (In_Free);
         else
            Free_Ranges (In_Free).Base := Cursor;
         end if;
      else
         Next_Page := Cursor;
      end if;
      Invalidate_Cache;

      Item.ELF_Bound := Cursor;
      Success := True;
   end Write_Image;

   ----------------
   -- Zero_Pages --
   ----------------

   procedure Zero_Pages (Base, Bound : Rose.Objects.Object_Id) is
      use type Rose.Objects.Object_Id;
      Page : Rose.Objects.Object_Id := Base;
   begin
      while Page < Bound loop
         begin
            Region.Put (Page, Zero_Page);
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;
         Page := Page + 1;
      end loop;
   end Zero_Pages;

end Exec.Library;
