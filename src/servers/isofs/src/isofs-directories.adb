with Rose.Words;

with Rose.System_Calls.Server;

with Rose.Console_IO;

with Rose.Interfaces.Stream_Reader;

with Rose.Containers.Bounded_Hashed_Maps;
with Rose.Containers.Queues;

package body IsoFS.Directories is

   use Rose.Interfaces.Block_Device;
   use Rose.Words;

   Max_Directories : constant := 100;
   Max_Open_Files  : constant := 20;

   Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client;

   subtype ISO_Sector is
     System.Storage_Elements.Storage_Array
       (1 .. ISO_Sector_Size);

   function To_Hash_Type
     (Sector : Rose.Words.Word_32)
      return Rose.Containers.Hash_Type
   is (Rose.Containers.Hash_Type (Sector));

   type Cache_Element is
      record
         Tick   : Rose.Words.Word_32;
         Sector : ISO_Sector;
      end record;

   package Sector_Cache is
     new Rose.Containers.Bounded_Hashed_Maps
       (Capacity     => 100,
        Modulus      => 317,
        Key_Type     => Rose.Words.Word_32,
        Element_Type => Cache_Element,
        Hash         => To_Hash_Type);

   package Sector_Cache_Queues is
     new Rose.Containers.Queues
       (Rose.Words.Word_32,
        Sector_Cache.Cursor,
        Rose.Words."<",
        Sector_Cache."=");

   Sector_Queue : Sector_Cache_Queues.Queue (100);
   Next_Tick    : Word_32 := 0;

   procedure Read_Sector
     (Address : Rose.Interfaces.Block_Device.Block_Address_Type;
      Sector  : out ISO_Sector);

   type Directory_Date_Time is array (1 .. 7) of Word_8;

   type Directory_Entry is
      record
         Length                           : Word_8;
         Extended_Attribute_Record_Length : Word_8;
         Extent_Location_LSB              : Word_32;
         Extent_Location_MSB              : Word_32;
         Extent_Size_LSB                  : Word_32;
         Extent_Size_MSB                  : Word_32;
         Recording_Date_Time              : Directory_Date_Time;
         File_Flags                       : Word_8;
         Interleaved_File_Unit_Size       : Word_8;
         Interleaved_Gap_Size             : Word_8;
         Volume_Sequence_LSB              : Word_16;
         Volume_Sequence_MSB              : Word_16;
         File_Identifier_Length           : Word_8;
      end record
   with Pack, Size => 33 * 8;

   type Primary_Volume_Sector is
      record
         Sector_Type                   : Word_8;
         Standard_Identifier           : String (1 .. 5);
         Version                       : Word_8;
         Unused_1                      : Word_8;
         System_Identifier             : String (1 .. 32);
         Volume_Identifier             : String (1 .. 32);
         Unused_2                      : Word_64;
         Volume_Space_Size_LSB         : Word_32;
         Volume_Space_Size_MSB         : Word_32;
         Unused_3                      : String (1 .. 32);
         Volume_Set_Size_LSB           : Word_16;
         Volume_Set_Size_MSB           : Word_16;
         Volume_Seq_Nr_LSB             : Word_16;
         Volume_Seq_Nr_MSB             : Word_16;
         Logical_Block_Size_LSB        : Word_16;
         Logical_Block_Size_MSB        : Word_16;
         Path_Table_Size_LSB           : Word_32;
         Path_Table_Size_MSB           : Word_32;
         L_Path_Table_Address          : Word_32;
         Optional_L_Path_Table_Address : Word_32;
         M_Path_Table_Address          : Word_32;
         Optional_M_Path_Table_Address : Word_32;
         Root_Directory_Entry          : Directory_Entry;
         Empty_Root_Directory_Name     : Word_8;
         Volume_Set_Identifier         : String (1 .. 128);
         Publisher_Identifier          : String (1 .. 128);
         Data_Preparer_Identifier      : String (1 .. 128);
         Application_Identifier        : String (1 .. 128);
         Copyright_File_Identifier     : String (1 .. 38);
         Abstract_File_Identifier      : String (1 .. 36);
         Bibliographic_File_Identifier : String (1 .. 37);
         Create_Date_Time              : String (1 .. 17);
         Modification_Date_Time        : String (1 .. 17);
         Expiration_Date_Time          : String (1 .. 17);
         Effective_Date_Time           : String (1 .. 17);
         File_Structure_Version        : Word_8;
         Unused_4                      : Word_8;
         Available                     : String (1 .. 512);
         Reserved                      : String (1 .. 653);
      end record
     with Pack, Size => 2048 * 8;

   type Directory_Caps_Record is
      record
         Valid, Filled         : Boolean := False;
         First_Child           : Directory_Type := No_Directory;
         Parent                : Directory_Type := No_Directory;
         Parent_Index          : Natural := 0;
         Child_Count           : Natural := 0;
         Sector_Address        : Block_Address_Type;
         Entry_Record          : Directory_Entry;
         Directory_Entry_Count : Rose.Capabilities.Capability := 0;
         Directory_Entry_Name  : Rose.Capabilities.Capability := 0;
         Directory_Entry_Kind  : Rose.Capabilities.Capability := 0;
         Directory_Entry_Size  : Rose.Capabilities.Capability := 0;
         Find_Entry            : Rose.Capabilities.Capability := 0;
         Get_Ordinary_File     : Rose.Capabilities.Capability := 0;
         Get_Directory         : Rose.Capabilities.Capability := 0;
         Read_File             : Rose.Capabilities.Capability := 0;
      end record;

   type Directory_Caps_Array is
     array (Directory_Type range 1 .. Max_Directories)
     of Directory_Caps_Record;

   Directory_Caps : Directory_Caps_Array;
   Directory_Count : Directory_Type := 0;

   type Open_File_Record is
      record
         Open          : Boolean := False;
         Start_Address : Rose.Words.Word := 0;
         Length        : Rose.Words.Word := 0;
         Current       : Rose.Words.Word := 0;
      end record;

   type Open_File_Array is array (1 .. Max_Open_Files) of Open_File_Record;

   Open_Files      : Open_File_Array;
   Open_File_Count : Natural := 0;

   procedure Read_Root_Directory
     (Device : Client.Block_Device_Client);

   function New_Cap
     (Directory : Directory_Type;
      Endpoint  : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability
   is (Rose.System_Calls.Server.Create_Endpoint
       (Create_Endpoint_Cap, Endpoint,
        Rose.Objects.Capability_Identifier (Directory)));

   procedure Create_Cap_Record
     (Directory : Directory_Type;
      Sector    : Block_Address_Type;
      Dir_Entry : Directory_Entry);

   procedure Check_Cap_Record
     (Directory : Directory_Type);

   procedure Scan_Directory_Entries
     (Directory : Directory_Type;
      Process   : not null access
        procedure (Rec : Directory_Entry;
                   Name : String));

   procedure Get_Child_Entry
     (Parent      : Directory_Type;
      Child_Index : Positive;
      Child_Entry : out Directory_Entry);

--     function Get_Directory_From_Address
--       (Address : Rose.Devices.Block.Block_Address_Type)
--        return Directory_Type;

   ----------------------
   -- Check_Cap_Record --
   ----------------------

   procedure Check_Cap_Record
     (Directory : Directory_Type)
   is
      Caps : Directory_Caps_Record renames Directory_Caps (Directory);
   begin
      if not Caps.Filled then
         declare
            Child_Entry : Directory_Entry;
         begin
            Get_Child_Entry (Caps.Parent, Caps.Parent_Index, Child_Entry);
            Create_Cap_Record
              (Directory, Caps.Sector_Address, Child_Entry);
         end;

      end if;
   end Check_Cap_Record;

   -----------------------
   -- Create_Cap_Record --
   -----------------------

   procedure Create_Cap_Record
     (Directory : Directory_Type;
      Sector    : Block_Address_Type;
      Dir_Entry : Directory_Entry)
   is
      use Rose.Interfaces.Directory;

      Caps : Directory_Caps_Record renames
               Directory_Caps (Directory);

      function New_Cap
        (EP : Rose.Objects.Endpoint_Id)
               return Rose.Capabilities.Capability
      is (New_Cap (Directory, EP));

      procedure Add_Child_Entry
        (Rec  : Directory_Entry;
         Name : String);

      ---------------------
      -- Add_Child_Entry --
      ---------------------

      procedure Add_Child_Entry
        (Rec  : Directory_Entry;
         Name : String)
      is
         pragma Unreferenced (Name);
         New_D    : constant Directory_Type :=
                      Directory_Count + 1;
         New_Caps : Directory_Caps_Record renames Directory_Caps (New_D);
      begin
         if Caps.First_Child = No_Directory then
            Caps.First_Child := New_D;
         end if;
         New_Caps.Valid := True;
         New_Caps.Sector_Address :=
           Block_Address_Type (Rec.Extent_Location_LSB);
         New_Caps.Parent := Directory;

         Directory_Count := Directory_Count + 1;
         Caps.Child_Count := Caps.Child_Count + 1;
         New_Caps.Parent_Index := Caps.Child_Count;
      end Add_Child_Entry;

   begin
      Caps :=
        (Caps with delta
           Filled                => True,
           Entry_Record          => Dir_Entry,
           First_Child           => No_Directory,
           Child_Count           => 0,
           SectOr_Address        => Sector,
           Directory_Entry_Count =>
              New_Cap (Directory_Entry_Count_Endpoint),
           Directory_Entry_Name  =>
              New_Cap (Directory_Entry_Name_Endpoint),
           Directory_Entry_Kind  =>
              New_Cap (Directory_Entry_Kind_Endpoint),
           Directory_Entry_Size  =>
             New_Cap (Directory_Entry_Size_Endpoint),
           Find_Entry            =>
             New_Cap (Find_Entry_Endpoint),
           Get_Ordinary_File     =>
              New_Cap (Get_Ordinary_File_Endpoint),
           Get_Directory         =>
              New_Cap (Get_Directory_Endpoint),
           Read_File             =>
             New_Cap (Read_File_Endpoint));

      Scan_Directory_Entries (Directory, Add_Child_Entry'Access);

   end Create_Cap_Record;

   -------------------------
   -- Get_Child_Directory --
   -------------------------

   function Get_Child_Directory
     (Parent     : Directory_Type;
      Child_Name : String)
      return Directory_Type
   is
      pragma Unreferenced (Parent, Child_Name);
   begin
      return No_Directory;
   end Get_Child_Directory;

   -------------------------
   -- Get_Child_Directory --
   -------------------------

   function Get_Child_Directory
     (Parent : Directory_Type;
      Index  : Positive)
      return Directory_Type
   is
   begin
      if Index <= Directory_Caps (Parent).Child_Count then
         declare
            Child : constant Directory_Type :=
                      Directory_Caps (Parent).First_Child
                      + Directory_Type (Index) - 1;
         begin
            Check_Cap_Record (Child);
            return Child;
         end;
      else
         return No_Directory;
      end if;
   end Get_Child_Directory;

   ---------------------
   -- Get_Child_Entry --
   ---------------------

   procedure Get_Child_Entry
     (Parent      : Directory_Type;
      Child_Index : Positive;
      Child_Entry : out Directory_Entry)
   is
      Count : Natural := 0;

      procedure Copy_Entry
        (Rec  : Directory_Entry;
         Name : String);

      -------------
      -- Process --
      -------------

      procedure Copy_Entry
        (Rec  : Directory_Entry;
         Name : String)
      is
         pragma Unreferenced (Name);
      begin
         Count := Count + 1;
         if Count = Child_Index then
            Child_Entry := Rec;
         end if;
      end Copy_Entry;

   begin
      Scan_Directory_Entries (Parent, Copy_Entry'Access);
   end Get_Child_Entry;

   ---------------------
   -- Get_Entry_Count --
   ---------------------

   function Get_Entry_Count
     (Directory : Directory_Type)
      return Natural
   is
      Count : Natural := 0;

      procedure Process
        (Rec  : Directory_Entry;
         Name : String);

      -------------
      -- Process --
      -------------

      procedure Process
        (Rec  : Directory_Entry;
         Name : String)
      is
         pragma Unreferenced (Rec, Name);
      begin
         Count := Count + 1;
      end Process;

   begin
      Scan_Directory_Entries (Directory, Process'Access);
      return Count;
   end Get_Entry_Count;

   --------------------
   -- Get_Entry_Kind --
   --------------------

   function Get_Entry_Kind
     (Directory : Directory_Type;
      Index     : Positive)
      return Rose.Interfaces.Directory.File_Kind
   is
      Count : Natural := 0;
      Kind  : Rose.Interfaces.Directory.File_Kind :=
                Rose.Interfaces.Directory.Ordinary_File;

      procedure Process
        (Rec        : Directory_Entry;
         Entry_Name : String);

      -------------
      -- Process --
      -------------

      procedure Process
        (Rec        : Directory_Entry;
         Entry_Name : String)
      is
         pragma Unreferenced (Entry_Name);
      begin
         Count := Count + 1;
         if Count = Index then
            if (Rec.File_Flags and 2) = 2 then
               Kind := Rose.Interfaces.Directory.Directory;
            else
               Kind := Rose.Interfaces.Directory.Ordinary_File;
            end if;
         end if;
      end Process;

   begin
      Scan_Directory_Entries (Directory, Process'Access);
      return Kind;
   end Get_Entry_Kind;

   --------------------
   -- Get_Entry_Name --
   --------------------

   procedure Get_Entry_Name
     (Directory : Directory_Type;
      Index     : Positive;
      Name      : out String;
      Last      : out Natural)
   is
      Count : Natural := 0;

      procedure Process
        (Rec        : Directory_Entry;
         Entry_Name : String);

      -------------
      -- Process --
      -------------

      procedure Process
        (Rec        : Directory_Entry;
         Entry_Name : String)
      is
         pragma Unreferenced (Rec);
      begin
         Count := Count + 1;
         if Count = Index then
            if Index <= 2 then
               if Name'Length > 0 then
                  Name (Name'First) := '.';
                  Last := Name'First;
               end if;
               if Index = 2 and then Name'Length > 1 then
                  Name (Name'First + 1) := '.';
                  Last := Name'First + 1;
               end if;
            else
               declare
                  Length : constant Natural :=
                             Natural'Min (Entry_Name'Length,
                                          Name'Length);
               begin
                  for I in 1 .. Length loop
                     Name (I - Name'First + 1) :=
                       Entry_Name (I - Entry_Name'First + 1);
                  end loop;
                  Last := Name'First + Length - 1;
               end;
            end if;
         end if;
      end Process;

   begin
      Last := 0;
      Scan_Directory_Entries (Directory, Process'Access);
   end Get_Entry_Name;

   ------------------------------
   -- Get_Identified_Directory --
   ------------------------------

   function Get_Identified_Directory
     (Identifier : Rose.Objects.Capability_Identifier)
      return Directory_Type
   is
      Directory : constant Directory_Type := Directory_Type (Identifier);
   begin
      if Directory in 1 .. Max_Directories
        and then Directory_Caps (Directory).Valid
      then
         return Directory;
      else
         return No_Directory;
      end if;
   end Get_Identified_Directory;

   -----------------------
   -- Get_Index_By_Name --
   -----------------------

   function Get_Index_By_Name
     (Directory : Directory_Type;
      Name      : String)
     return Natural
   is
      Count  : Natural := 0;
      Result : Natural := 0;

      procedure Process
        (Rec        : Directory_Entry;
         Entry_Name : String);

      -------------
      -- Process --
      -------------

      procedure Process
        (Rec        : Directory_Entry;
         Entry_Name : String)
      is
         pragma Unreferenced (Rec);
      begin
         Count := Count + 1;
         if Count > 2
           and then Result = 0
         then
            if Result = 0
              and then Entry_Name'Length = Name'Length
            then
               declare
                  Match : Boolean := True;
               begin
                  for I in Entry_Name'Range loop
                     if Entry_Name (I) /= Name (I - Name'First + 1) then
                        Match := False;
                        exit;
                     end if;
                  end loop;
                  if Match then
                     Result := Count;
                  end if;
               end;
            end if;
         end if;
      end Process;

   begin
      if Name = "." then
         return 1;
      elsif Name = ".." then
         return 2;
      end if;

      Scan_Directory_Entries (Directory, Process'Access);
      return Result;
   end Get_Index_By_Name;

   ------------------------
   -- Get_Root_Directory --
   ------------------------

   function Get_Root_Directory
     (Device : Client.Block_Device_Client)
      return Directory_Type
   is
   begin
      if not Directory_Caps (Root_Directory).Valid then
         Read_Root_Directory (Device);
      end if;
      if Directory_Caps (Root_Directory).Valid then
         return Root_Directory;
      else
         return No_Directory;
      end if;
   end Get_Root_Directory;

   ----------
   -- Read --
   ----------

   procedure Read
     (File   : Positive;
      Buffer : out System.Storage_Elements.Storage_Array;
      Count  : out System.Storage_Elements.Storage_Count)
   is
      use System.Storage_Elements;
      F              : Open_File_Record renames Open_Files (File);
      Start_Sector   : Word;
      Current_Sector : Word;
      Current_Offset : Storage_Offset;
      Sector_Data    : ISO_Sector;
   begin
      if not F.Open then
         Count := 0;
         return;
      end if;

      Start_Sector := F.Start_Address + F.Current / ISO_Sector_Size;
      Current_Sector := Start_Sector;
      Current_Offset := Storage_Offset (F.Current mod ISO_Sector_Size) + 1;

      Count := Storage_Count'Min (Buffer'Length,
                                  Storage_Count (F.Length - F.Current));

      Read_Sector (Block_Address_Type (Current_Sector), Sector_Data);

      for I in 1 .. Count loop
         Buffer (Buffer'First + I - 1) :=
           Sector_Data (Storage_Count (Current_Offset));
         Current_Offset := Current_Offset + 1;
         if Current_Offset > ISO_Sector_Size then
            Current_Offset := 1;
            Current_Sector := Current_Sector + 1;
            Read_Sector (Block_Address_Type (Current_Sector), Sector_Data);
         end if;
      end loop;

      F.Current := F.Current + Word (Count);

      if Count < Buffer'Length then
         F.Open := False;
         if File = Open_File_Count then
            Open_File_Count := Open_File_Count - 1;
         end if;
      end if;

   end Read;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Directory : Directory_Type;
      Index     : Positive)
      return Rose.Capabilities.Capability
   is
      Count : Natural := 0;

      procedure Process
        (Rec        : Directory_Entry;
         Entry_Name : String);

      -------------
      -- Process --
      -------------

      procedure Process
        (Rec        : Directory_Entry;
         Entry_Name : String)
      is
         pragma Unreferenced (Entry_Name);
      begin
         Count := Count + 1;
         if Count = Index then
            Open_File_Count := Open_File_Count + 1;
            Open_Files (Open_File_Count) :=
              Open_File_Record'
                (Open          => True,
                 Start_Address => Rec.Extent_Location_LSB,
                 Length        => Rec.Extent_Size_LSB,
                 Current       => 0);
         end if;
      end Process;

   begin
      if Open_File_Count >= Max_Open_Files then
         return Rose.Capabilities.Null_Capability;
      end if;

      Scan_Directory_Entries (Directory, Process'Access);

      return Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap  => Create_Endpoint_Cap,
         Endpoint_Id => Rose.Interfaces.Stream_Reader.Read_Endpoint,
         Identifier  => Rose.Objects.Capability_Identifier (Open_File_Count));

   end Read_File;

   -------------------------
   -- Read_Root_Directory --
   -------------------------

   procedure Read_Root_Directory
     (Device : Client.Block_Device_Client)
   is
      use System.Storage_Elements;
      use Rose.Interfaces.Block_Device.Client;

      Volume_Index : Block_Address_Type := ISO_First_Volume_Sector;
      Sector_Count : Block_Address_Type;
      Sector_Size  : Block_Size_Type;
      Found        : Boolean := False;
      Buffer       : ISO_Sector;
   begin

      Get_Parameters (Device, Sector_Count, Sector_Size);

      Block_Device := Device;

      while Volume_Index < Sector_Count
        and then not Found
      loop

         Read_Sector (Volume_Index, Buffer);

         if Buffer (Descriptor_Type_Offset + 1)
           = Primary_Volume_Descriptor
         then
            Found := True;
         else
            Volume_Index := Volume_Index + 1;
         end if;
      end loop;

      if not Found then
         Rose.Console_IO.Put_Line
           ("isofs: cannot find primary volume descriptor");
         return;
      end if;

      declare
         Volume : Primary_Volume_Sector;
         pragma Import (Ada, Volume);
         for Volume'Address use Buffer'Address;
      begin
         Create_Cap_Record
           (Root_Directory, Volume_Index, Volume.Root_Directory_Entry);
      end;

   end Read_Root_Directory;

   -----------------
   -- Read_Sector --
   -----------------

   procedure Read_Sector
     (Address : Rose.Interfaces.Block_Device.Block_Address_Type;
      Sector  : out ISO_Sector)
   is
      use Sector_Cache_Queues;
      Key : constant Rose.Words.Word_32 := Rose.Words.Word_32 (Address);
      Position : Sector_Cache.Cursor :=
                   Sector_Cache.Find (Key);
      Item     : Cache_Element;

   begin

      Next_Tick := Next_Tick + 1;

      if not Sector_Cache.Has_Element (Position) then
         if Sector_Cache.Is_Full then
            declare
               Oldest : Sector_Cache.Cursor :=
                          First_Element (Sector_Queue);
            begin
               Sector_Cache.Delete (Oldest);
               Delete_First (Sector_Queue);
            end;
         end if;

         Rose.Interfaces.Block_Device.Client.Read_Blocks
           (Block_Device, Address, 1, Sector);

         Item := (Next_Tick, Sector);

         declare
            Inserted : Boolean;
            pragma Unreferenced (Inserted);
         begin
            Sector_Cache.Insert
              (Key       => Key,
               New_Item  => Item,
               Position  => Position,
               Inserted  => Inserted);
         end;
      else

         Item := Sector_Cache.Element (Position);
         Delete (Sector_Queue, Item.Tick);
         Item.Tick := Next_Tick;
         Sector_Cache.Replace_Element (Position, Item);
         Sector := Item.Sector;

      end if;

      Insert (Sector_Queue, Next_Tick, Position);

   end Read_Sector;

   ----------------------------
   -- Scan_Directory_Entries --
   ----------------------------

   procedure Scan_Directory_Entries
     (Directory : Directory_Type;
      Process   : not null access
        procedure (Rec : Directory_Entry;
                   Name : String))
   is
      use System.Storage_Elements;
      Rec          : Directory_Caps_Record renames Directory_Caps (Directory);
      Dir          : Directory_Entry renames Rec.Entry_Record;
      Location     : Word_32 := Dir.Extent_Location_LSB;
      Length       : constant Word_32 := Dir.Extent_Size_LSB;
      Sector       : ISO_Sector;
      Have_Sector  : Boolean := False;
      Position     : Storage_Offset := 1;
      Sector_Start : Storage_Offset;
   begin
      while Position < Storage_Count (Length) loop
         if not Have_Sector then
            Read_Sector (Block_Address_Type (Location), Sector);
            Have_Sector := True;
            Sector_Start := Position - 1;
         end if;

         exit when Sector (Position - Sector_Start) = 0;

         declare
            Start         : constant Storage_Offset := Position - Sector_Start;
            Record_Length : constant Storage_Count :=
                              Storage_Count (Sector (Start));
            Rec           : Directory_Entry;
            pragma Import (Ada, Rec);
            for Rec'Address use Sector (Start)'Address;

            Name : String (1 .. Natural (Rec.File_Identifier_Length));
            Last : Natural := Name'Last;
         begin

            for I in Name'Range loop
               declare
                  Index : constant Storage_Offset :=
                            Start + 32 + Storage_Count (I);
               begin
                  Name (I) := Character'Val (Sector (Index));
                  if Name (I) = ';' then
                     Last := I - 1;
                     exit;
                  end if;
               end;
            end loop;

            if Name (Last) = '.' then
               Last := Last - 1;
            end if;

            Process (Rec, Name (1 .. Last));

            Position := Position + Record_Length;
            if Position - Sector_Start >= Sector'Last then
               Location := Location + 1;
               Have_Sector := False;
            end if;
         end;

      end loop;
   end Scan_Directory_Entries;

   -------------------------
   -- Send_Directory_Caps --
   -------------------------

   procedure Send_Directory_Caps
     (Directory             : Directory_Type;
      Params                : in out Rose.Invocation.Invocation_Record)
   is
      use Rose.System_Calls;
      Caps : Directory_Caps_Record renames Directory_Caps (Directory);
   begin
      Send_Cap (Params, Caps.Directory_Entry_Count);
      Send_Cap (Params, Caps.Directory_Entry_Name);
      Send_Cap (Params, Caps.Directory_Entry_Kind);
      Send_Cap (Params, Caps.Directory_Entry_Size);
      Send_Cap (Params, Caps.Find_Entry);
      Send_Cap (Params, Caps.Get_Ordinary_File);
      Send_Cap (Params, Caps.Get_Directory);
      Send_Cap (Params, Caps.Read_File);
      Send_Cap (Params, Rose.Capabilities.Null_Capability);
      Send_Cap (Params, Rose.Capabilities.Null_Capability);
   end Send_Directory_Caps;

end IsoFS.Directories;
