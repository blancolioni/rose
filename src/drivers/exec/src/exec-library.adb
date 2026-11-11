with Ada.Containers.Vectors;
with Ada.Streams;
with System.Storage_Elements;

with Rose.Limits;

with Rose.Interfaces.Region.Client;

with Exec.Output;

package body Exec.Library is

   use System.Storage_Elements;

   Max_Start_Caps         : constant := 16;
   Max_Name_Length        : constant := 16;

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
         Metadata_Offset : Storage_Count := 0;
         Metadata_Length : Storage_Count := 0;
         Retired         : Boolean := False;
         --  A newer version replaced it (Update). Its pages are kept,
         --  because the processes still running it map them.
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

   Region         : Rose.Interfaces.Region.Client.Region_Client;
   --  Where the images are kept.
   Page_Base,
   Page_Bound         : Rose.Objects.Object_Id := 0;
   Next_Page          : Rose.Objects.Object_Id := 0;

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

   function Padded (Name : String) return Installed_Program_Name;
   --  Name as a program record holds it: cut, or padded with spaces.

   procedure Write_Image
     (ELF_Image : in out Rose.Interfaces.Reader.Reader_Object'Class;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array;
      Old       : Natural;
      Item      : out Installed_Program_Record;
      Same      : out Boolean;
      Success   : out Boolean);
   --  Copy ELF_Image into the region's free pages, and make Item the
   --  program record for it, which is not added yet. Same is True if Old
   --  is a program whose image is the same, page for page, and neither
   --  has install caps. Success is False, and nothing is kept, if the
   --  image doesn't fit.

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
            Image : Rose.Interfaces.Region.Region_Object'Class :=
              Region.Create_Subregion (Item.ELF_Base, Item.ELF_Bound, 5);
         begin
            Item.Image := Image.Region_Cap;
         end;
      end if;
      return Item.Image;
   end Image_Region;

   -------------
   -- Install --
   -------------

   function Install
     (ELF_Image : in out Rose.Interfaces.Reader.Reader_Object'Class;
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

   ----------------
   -- Is_Retired --
   ----------------

   function Is_Retired
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean
   is (not Is_Installed (Id) or else Program (Id).Retired);

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
      Valid := Offset <= Image_Size
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

   -----------------------
   -- Send_Install_Caps --
   -----------------------

   procedure Send_Install_Caps
     (Id     : Rose.Objects.Capability_Identifier;
      Params : in out Rose.Invocation.Invocation_Record)
   is
      use type Rose.Capabilities.Capability;
   begin
      if Is_Installed (Id) then
         for Cap of Program (Id).Caps loop
            exit when Cap = 0;
            Rose.Invocation.Send_Cap (Params, Cap);
         end loop;
      end if;
   end Send_Install_Caps;

   --------------------
   -- Set_Region --
   --------------------

   procedure Set_Region (Region : Rose.Capabilities.Capability) is
   begin
      Rose.Interfaces.Region.Client.Open (Exec.Library.Region, Region);
      Exec.Library.Region.Get_Range (Page_Base, Page_Bound);
      Next_Page := Page_Base;
   end Set_Region;

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
     (ELF_Image : in out Rose.Interfaces.Reader.Reader_Object'Class;
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

         --  The pages just written are the installed version's again:
         --  give them back, so that the region doesn't grow.

         Next_Page := Item.ELF_Base;
         return Rose.Objects.Capability_Identifier (Old);
      end if;

      Find_Metadata (Item);
      Installed_Programs.Append (Item);
      Changed := True;

      if Old /= 0 then
         Installed_Programs (Old).Retired := True;
      end if;

      return Rose.Objects.Capability_Identifier
        (Installed_Programs.Last_Index);
   end Update;

   -----------------
   -- Write_Image --
   -----------------

   procedure Write_Image
     (ELF_Image : in out Rose.Interfaces.Reader.Reader_Object'Class;
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
   begin
      Item.Caps := (others => 0);
      Item.Caps (1 .. Caps'Length) := Caps;
      Item.Name := Padded (Name);
      Item.ELF_Base := Next_Page;

      --  Install caps can't be compared, since a cap's number says
      --  nothing about what it is, so a program with any is changed.

      Same := Old /= 0
        and then Caps'Length = 0
        and then Installed_Programs (Old).Caps (1) = 0;

      if Old /= 0 then
         Old_Base := Installed_Programs (Old).ELF_Base;
         Old_Bound := Installed_Programs (Old).ELF_Bound;
      end if;

      loop
         ELF_Image.Read (Data, Last);
         exit when Last < Data'First;

         --  An image that doesn't fit is not installed, rather than
         --  installed with its last pages missing (#669).

         if Next_Page >= Page_Bound then
            Exec.Output.Put_Line ("exec: no room for " & Name);
            Next_Page := Item.ELF_Base;
            Same := False;
            Success := False;
            return;
         end if;

         Buffer (Storage_Offset (Last) + 1 .. Buffer'Last) :=
           (others => 0);
         Region.Put (Next_Page, Buffer);

         if Same then
            if Old_Base + Page_Index < Old_Bound then
               Region.Get (Old_Base + Page_Index, Old_Page);
               Same := Old_Page = Buffer;
            else
               Same := False;
            end if;
         end if;

         Next_Page := Next_Page + 1;
         Page_Index := Page_Index + 1;
      end loop;

      Item.ELF_Bound := Next_Page;
      Same := Same and then Old_Base + Page_Index = Old_Bound;
      Success := True;
   end Write_Image;

end Exec.Library;
