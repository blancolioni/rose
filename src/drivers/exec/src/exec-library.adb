with System.Storage_Elements;

with Rose.Limits;
with Rose.System_Calls.Server;

with Rose.Interfaces.Launch;

package body Exec.Library is

   use System.Storage_Elements;

   Max_Installed_Programs : constant := 100;
   Max_Start_Caps         : constant := 16;
   Max_Name_Length        : constant := 16;

   subtype Installed_Program_Name is String (1 .. Max_Name_Length);

   subtype Interface_Caps is
     Rose.Capabilities.Capability_Array
       (1 .. Rose.Interfaces.Launch.Subprogram_Count);
   --  The caps of a program's Launch interface: each at its index in
   --  Rose.Interfaces.Launch, plus one.

   type Installed_Program_Record is
      record
         Installed       : Boolean := False;
         Caps            : Rose.Capabilities.Capability_Array
                             (1 .. Max_Start_Caps);
         Name            : Installed_Program_Name;
         ELF_Base        : Rose.Objects.Page_Object_Id;
         ELF_Bound       : Rose.Objects.Page_Object_Id;
         Launch_Caps     : Interface_Caps := (others => 0);
         Have_Image      : Boolean := False;
         Image           : Rose.Interfaces.Region.Client.Region_Client;
         Metadata_Offset : Storage_Count := 0;
         Metadata_Length : Storage_Count := 0;
      end record;

   subtype Installed_Program_Index is
     Rose.Objects.Capability_Identifier range 1 .. Max_Installed_Programs;

   type Installed_Program_Array is
     array (Installed_Program_Index) of Installed_Program_Record;

   Installed_Programs : Installed_Program_Array;
   Installed_Count    : Rose.Objects.Capability_Identifier := 0;

   Region         : Rose.Interfaces.Region.Client.Region_Client;
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

      if Id not in Installed_Program_Index
        or else not Installed_Programs (Id).Installed
        or else Installed_Programs (Id).Metadata_Length = 0
      then
         return;
      end if;

      declare
         Program : Installed_Program_Record renames Installed_Programs (Id);
         Length  : constant Storage_Count :=
           Storage_Count'Min
             (Program.Metadata_Length, Storage_Count (Result'Length));
         Data    : Storage_Array (1 .. Length);
         Valid   : Boolean;
      begin
         Read_Image (Program, Program.Metadata_Offset, Data, Valid);
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
      if Id not in Installed_Program_Index
        or else not Installed_Programs (Id).Installed
      then
         return;
      end if;

      for Ch of Installed_Programs (Id).Name loop
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
      return Rose.Interfaces.Region.Client.Region_Client
   is
      Program : Installed_Program_Record renames Installed_Programs (Id);
   begin
      if not Program.Have_Image then
         Program.Image :=
           Rose.Interfaces.Region.Client.Create_Subregion
             (Region, Program.ELF_Base, Program.ELF_Bound, 5);
         Program.Have_Image := True;
      end if;
      return Program.Image;
   end Image_Region;

   -------------
   -- Install --
   -------------

   function Install
     (ELF_Image : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      if Installed_Count = Max_Installed_Programs then
         return 0;
      end if;

      Installed_Count := Installed_Count + 1;

      declare
         use Rose.Interfaces.Stream_Reader.Client;
         use Rose.Interfaces.Region.Client;
         use type Rose.Objects.Object_Id;
         Buffer : Page_Buffer;
         Last   : Storage_Count;
         Install : Installed_Program_Record renames
                     Installed_Programs (Installed_Count);
      begin
         Install.Installed := True;
         Install.Caps := (others => 0);
         Install.Caps (1 .. Caps'Length) := Caps;

         Install.Name := (others => ' ');

         if Name'Length <= Max_Name_Length then
            Install.Name (1 .. Name'Length) := Name;
         else
            Install.Name :=
              Name (Name'First .. Name'First + Max_Name_Length - 1);
         end if;

         Install.ELF_Base := Next_Page;

         loop
            Read (ELF_Image, Buffer, Last);
            exit when Last = 0;
            Buffer (Last + 1 .. Buffer'Last) := (others => 0);
            Put (Region, Next_Page, Buffer);
            Next_Page := Next_Page + 1;
         end loop;
         Install.ELF_Bound := Next_Page;

         Find_Metadata (Install);
      end;

      return Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap  => Create_Endpoint_Cap,
         Endpoint_Id => Rose.Interfaces.Launch.Launch_Interface,
         Identifier  => Installed_Count);
   end Install;

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
               Rose.Interfaces.Region.Client.Get
                 (Region, Page, Cache (Cached).Data);
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
      if Id in Installed_Program_Index
        and then Installed_Programs (Id).Installed
      then
         for Cap of Installed_Programs (Id).Caps loop
            exit when Cap = 0;
            Rose.Invocation.Send_Cap (Params, Cap);
         end loop;
      end if;
   end Send_Install_Caps;

   --------------------
   -- Send_Interface --
   --------------------

   procedure Send_Interface
     (Id     : Rose.Objects.Capability_Identifier;
      Params : in out Rose.Invocation.Invocation_Record)
   is
      use type Rose.Capabilities.Capability;

      function Create
        (Endpoint : Rose.Objects.Endpoint_Id)
         return Rose.Capabilities.Capability
      is (Rose.System_Calls.Server.Create_Endpoint
            (Create_Cap  => Create_Endpoint_Cap,
             Endpoint_Id => Endpoint,
             Identifier  => Id));

   begin
      if Id not in Installed_Program_Index
        or else not Installed_Programs (Id).Installed
      then
         return;
      end if;

      declare
         Caps : Interface_Caps renames Installed_Programs (Id).Launch_Caps;
      begin
         if Caps (Caps'First) = 0 then
            Caps (Rose.Interfaces.Launch.Launch_Index + 1) :=
              Create (Rose.Interfaces.Launch.Launch_Endpoint);
            Caps (Rose.Interfaces.Launch.Launch_Command_Index + 1) :=
              Create (Rose.Interfaces.Launch.Launch_Command_Endpoint);
            Caps (Rose.Interfaces.Launch.Metadata_Index + 1) :=
              Create (Rose.Interfaces.Launch.Metadata_Endpoint);
         end if;

         for Cap of Caps loop
            Rose.Invocation.Send_Cap (Params, Cap);
         end loop;
      end;
   end Send_Interface;

   --------------------
   -- Set_Region --
   --------------------

   procedure Set_Region
     (Client : Rose.Interfaces.Region.Client.Region_Client)
   is
   begin
      Region := Client;
      Rose.Interfaces.Region.Client.Get_Range
        (Region, Page_Base, Page_Bound);
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

end Exec.Library;
