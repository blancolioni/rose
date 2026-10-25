with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Invocation;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Kernel_Log.Server;
with Rose.Interfaces.Log_Reader.Server;
with Rose.Interfaces.State.Server;

with Rose.Server;
with Rose.System_Calls.Client;

with Log.Header;

package body Log.Server is

   use type System.Storage_Elements.Storage_Count;

   use Rose.Interfaces.Block_Device;
   subtype Block_Device_Client is
     Rose.Interfaces.Block_Device.Client.Block_Device_Client;

   Max_Log_Devices : constant := 4;

   type Log_Device_Count is range 0 .. Max_Log_Devices;
   subtype Log_Device_Index is
     Log_Device_Count range 1 .. Log_Device_Count'Last;

   type Log_Device_Record is
      record
         Block_Count    : Block_Address_Type;
         Block_Size     : Block_Size_Type;
         Multiplier     : Positive;
         Client         : Block_Device_Client;
         First_Location : Log_Location_Id;
         Last_Location  : Log_Location_Id;
      end record;

   Log_Devices : array (Log_Device_Index) of Log_Device_Record;
   Last_Log_Device : Log_Device_Count := 0;

   Log_Server   : Rose.Server.Server_Context;

   Active             : Boolean := False;
   Writing            : Boolean := False;
   Entry_Failed       : Boolean := False;
   --  Set when the entry being written can't be completed, because the
   --  log is full or a write failed.  Commit then leaves it out of the
   --  log, so the previous entry is still the last one.

   Current_Log_Header : Log.Header.Entry_Header_Type;
   Header             : Log.Header.Log_Header_Type;
   Next_Log_Location  : Log_Location_Id;

   First_Committed    : Log_Location_Id := 0;
   Last_Committed     : Log_Location_Id := 0;
   --  The locations of the first and last committed entries, or 0 if
   --  there are none.  Readers only see entries in this range.

   Cached_Location    : Log_Location_Id := 0;
   Cached_Header      : Log.Header.Entry_Header_Type;
   --  The header of the committed entry at Cached_Location, if that is
   --  not 0, so that reading an entry's pages doesn't read its header
   --  each time.

   Log_Page_Buffer_Count : constant := 8;
   type Page_Buffer_Count is range 0 .. Log_Page_Buffer_Count;
   subtype Page_Buffer_Index is
     Page_Buffer_Count range 1 .. Page_Buffer_Count'Last;

   subtype Log_Page is
     System.Storage_Elements.Storage_Array (1 .. Log_Page_Size);

   Page_Buffer           : array (Page_Buffer_Index) of Log_Page;
   Page_Buffer_Last      : Page_Buffer_Count := 0;
   Page_Buffer_Loc       : Log_Location_Id;

   Invoke_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Log_Page_Buffer_Count * Log_Page_Size)
     with Alignment => 4096;

   procedure Flush;

   procedure Reset
     (Id          : in     Rose.Objects.Capability_Identifier);

   procedure Create
     (Id          : in     Rose.Objects.Capability_Identifier);

   procedure Commit
     (Id          : in     Rose.Objects.Capability_Identifier);

   procedure Append
     (Id   : Rose.Objects.Capability_Identifier;
      Data : System.Storage_Elements.Storage_Array);

   procedure Add_Log_Device
     (Id     : Rose.Objects.Capability_Identifier;
      Device : Rose.Capabilities.Capability);

   function Get_Device_Index
     (Location : Log_Location_Id)
      return Log_Device_Count;

   function Has_Room (Location : Log_Location_Id) return Boolean
   is (Last_Log_Device > 0
       and then Location <= Log_Devices (Last_Log_Device).Last_Location);
   --  True if Location is on a log device.

   function Is_Log_Entry
     (Location : Log_Location_Id;
      Entry_Header : Log.Header.Entry_Header_Type)
      return Boolean;
   --  True if Entry_Header, read from Location, is an entry of this
   --  generation of the log.

   procedure Fail_Entry (Message : String);
   --  Report Message, and leave the current entry out of the log.

   function Load_Entry (Location : Log_Location_Id) return Boolean;
   --  If Location is a committed entry, put its header in Cached_Header
   --  and return True.

   function First_Entry
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64;

   function Last_Entry
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64;

   function Next_Entry
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64)
      return Rose.Words.Word_64;

   function Page_Count
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64)
      return Natural;

   procedure Read_Page
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64;
      Index    : Positive;
      Page     : out System.Storage_Elements.Storage_Array);

   type Device_Operation is (Read, Write);

   procedure Operate
     (Location   : Log_Location_Id;
      Addr       : System.Address;
      Operation  : Device_Operation;
      Page_Count : Natural;
      Success    : out Boolean);

   procedure Read
     (Location : Log_Location_Id;
      Addr     : System.Address;
      Success  : out Boolean);

   procedure Write
     (Location : Log_Location_Id;
      Addr     : System.Address;
      Success  : out Boolean);

   --------------------
   -- Add_Log_Device --
   --------------------

   procedure Add_Log_Device
     (Id     : Rose.Objects.Capability_Identifier;
      Device : Rose.Capabilities.Capability)
   is
   begin
      if Last_Log_Device < Log_Device_Count'Last then
         Last_Log_Device := Last_Log_Device + 1;

         declare
            Client      : Block_Device_Client;
            Block_Count : Block_Address_Type;
            Block_Size  : Block_Size_Type;
            First_Location : Log_Location_Id := 0;
            Last_Location  : Log_Location_Id := 0;
            Page_Count     : Log_Location_Id := 0;
         begin
            Rose.Interfaces.Block_Device.Client.Open (Client, Device);
            Rose.Interfaces.Block_Device.Client.Get_Parameters
              (Item        => Client,
               Block_Count => Block_Count,
               Block_Size  => Block_Size);

            Page_Count := Log_Location_Id (Block_Count)
              * Log_Location_Id (Block_Size) / Log_Page_Size;

            if Last_Log_Device > 1 then
               First_Location :=
                 Log_Devices (Last_Log_Device - 1).Last_Location + 1;
            end if;

            Last_Location := First_Location + Page_Count - 1;

            Log_Devices (Last_Log_Device) :=
              Log_Device_Record'
                (Block_Count    => Block_Count,
                 Block_Size     => Block_Size,
                 Multiplier     =>
                   Natural'Max (1, 4096 / Natural (Block_Size)),
                 Client         => Client,
                 First_Location => First_Location,
                 Last_Location  => Last_Location);

            Rose.Console_IO.Put ("log: added device ");
            Rose.Console_IO.Put (Natural (Last_Log_Device));
            Rose.Console_IO.Put (": block size ");
            Rose.Console_IO.Put (Natural (Block_Size));
            Rose.Console_IO.Put ("/");
            Rose.Console_IO.Put (Log_Devices (Last_Log_Device).Multiplier);
            Rose.Console_IO.Put ("; count ");
            Rose.Console_IO.Put (Natural (Block_Count));
            Rose.Console_IO.Put ("; range ");
            Rose.Console_IO.Put (Natural (First_Location));
            Rose.Console_IO.Put ("..");
            Rose.Console_IO.Put (Natural (Last_Location));
            Rose.Console_IO.New_Line;

         end;

         if Last_Log_Device = 1 then

            declare
               use type Rose.Words.Word_64;
               Success : Boolean;
            begin
               Read (0, Header'Address, Success);
               if Success and then Header.Magic = Log.Header.Log_Magic then
                  Rose.Console_IO.Put_Line ("log: found valid log device");

                  Next_Log_Location := 1;
                  loop
                     if not Has_Room (Next_Log_Location) then
                        Rose.Console_IO.Put_Line ("log: log is full");
                        exit;
                     end if;

                     Read (Next_Log_Location, Current_Log_Header'Address,
                           Success);
                     exit when not Success
                       or else not Is_Log_Entry
                         (Next_Log_Location, Current_Log_Header);

                     Rose.Console_IO.Put
                       ("log: skipping entry at location ");
                     Rose.Console_IO.Put (Natural (Next_Log_Location));
                     Rose.Console_IO.New_Line;

                     if First_Committed = 0 then
                        First_Committed := Next_Log_Location;
                     end if;
                     Last_Committed := Next_Log_Location;

                     Next_Log_Location := Current_Log_Header.Log_Last + 1;
                  end loop;

                  Rose.Console_IO.Put
                    ("log: next log at location ");
                  Rose.Console_IO.Put (Natural (Next_Log_Location));
                  Rose.Console_IO.New_Line;

               else

                  Rose.Console_IO.Put_Line ("log: creating new log");
                  Reset (Id);

               end if;

               Active := True;

            end;

            declare
               use Rose.Interfaces.Kernel_Log.Server;
               Params : aliased Rose.Invocation.Invocation_Record;
            begin
               Rose.System_Calls.Initialize_Send
                 (Params, Start_Log_Cap);
               Rose.System_Calls.Send_Cap (Params, Get_Create_Cap);
               Rose.System_Calls.Send_Cap (Params, Get_Append_Cap);
               Rose.System_Calls.Send_Cap (Params, Get_Commit_Cap);
               Rose.System_Calls.Invoke_Capability (Params);
            end;

         end if;

         Rose.Console_IO.Put_Line ("log: ready");

      end if;
   end Add_Log_Device;

   ------------
   -- Append --
   ------------

   procedure Append
     (Id   : Rose.Objects.Capability_Identifier;
      Data : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
   begin
      if not Writing then
         Rose.Console_IO.Put_Line ("log: append: not currently logging");
         return;
      end if;

      if Entry_Failed then
         return;
      end if;

      if Data'Length /= Log_Page_Size then
         Rose.Console_IO.Put ("log: append: page is ");
         Rose.Console_IO.Put (Natural (Data'Length));
         Rose.Console_IO.Put (" bytes, expected ");
         Rose.Console_IO.Put (Natural (Log_Page_Size));
         Rose.Console_IO.New_Line;
         Fail_Entry ("log: append: bad page");
         return;
      end if;

      if not Has_Room (Current_Log_Header.Log_Last + 1) then
         Fail_Entry ("log: log is full");
         return;
      end if;

      Current_Log_Header.Log_Last :=
        Current_Log_Header.Log_Last + 1;

      if Page_Buffer_Last = Log_Page_Buffer_Count then
         Flush;
         if Entry_Failed then
            return;
         end if;
         Page_Buffer_Loc := Current_Log_Header.Log_Last;
      end if;

      Page_Buffer_Last := Page_Buffer_Last + 1;
      Page_Buffer (Page_Buffer_Last) := Data;

   end Append;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Id          : in     Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin

      if not Writing then
         Rose.Console_IO.Put_Line ("log: commit: not writing");
         return;
      end if;

      --  The entry's pages go to the disk before its header, so that a
      --  header is only ever found with all of its pages.

      if not Entry_Failed then
         Flush;
      end if;

      if not Entry_Failed then
         declare
            Success : Boolean;
         begin
            Write (Next_Log_Location, Current_Log_Header'Address, Success);
            if Success then
               if First_Committed = 0 then
                  First_Committed := Next_Log_Location;
               end if;
               Last_Committed := Next_Log_Location;
               Next_Log_Location := Current_Log_Header.Log_Last + 1;
            else
               Fail_Entry ("log: commit: cannot write entry header");
            end if;
         end;
      end if;

      if Entry_Failed then
         Rose.Console_IO.Put_Line ("log: checkpoint not committed");
      end if;

      Page_Buffer_Last := 0;
      Writing := False;

   end Commit;

   ------------
   -- Create --
   ------------

   procedure Create
     (Id : in Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin

      if not Active then
         Rose.Console_IO.Put_Line ("log: create: not active");
         return;
      end if;

      if Writing then
         Rose.Console_IO.Put_Line ("log: create: already writing");
         return;
      end if;

      Writing := True;
      Entry_Failed := False;
      --  The startup scan reads into Current_Log_Header, so after a
      --  restart it holds whatever page followed the last entry.  Set
      --  each field, rather than assigning a whole header: that would
      --  need a page-sized temporary on the stack.
      Current_Log_Header.Magic := Log.Header.Entry_Magic;
      Current_Log_Header.Version := Log_Version;
      Current_Log_Header.Flags := Log.Header.Is_Active_Entry;
      Current_Log_Header.Generation := Header.Generation;
      Current_Log_Header.Log_First := Next_Log_Location + 1;
      Current_Log_Header.Log_Last := Next_Log_Location;
      Page_Buffer_Last := 0;
      Page_Buffer_Loc := Current_Log_Header.Log_First;

      if not Has_Room (Next_Log_Location) then
         Fail_Entry ("log: log is full");
      end if;

   end Create;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is

      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));

   begin

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap);

      Rose.System_Calls.Use_Buffer
        (Buffer_Address => Invoke_Buffer'Address,
         Buffer_Size    => Invoke_Buffer'Length);

      Console_Cap := Get_Cap (1);
      Start_Log_Cap := Get_Cap (2);

      Rose.Console_IO.Open (Console_Cap);

      Rose.Console_IO.Put_Line ("log: starting");

      Rose.Interfaces.Kernel_Log.Server.Create_Server
        (Server_Context => Log_Server,
         Create         => Create'Access,
         Append         => Append'Access,
         Commit         => Commit'Access,
         Add_Log_Device => Add_Log_Device'Access,
         Instanced      => False);

      Rose.Interfaces.State.Server.Attach_Interface
        (Server_Context => Log_Server,
         Reset          => Reset'Access);

      Rose.Interfaces.Log_Reader.Server.Attach_Interface
        (Server_Context => Log_Server,
         First_Entry    => First_Entry'Access,
         Last_Entry     => Last_Entry'Access,
         Next_Entry     => Next_Entry'Access,
         Page_Count     => Page_Count'Access,
         Read_Page      => Read_Page'Access);

      for Page of Page_Buffer loop
         for Element of Page loop
            Element := 0;
         end loop;
      end loop;

   end Create_Server;

   ----------------
   -- Fail_Entry --
   ----------------

   procedure Fail_Entry (Message : String) is
   begin
      if not Entry_Failed then
         Rose.Console_IO.Put_Line (Message);
         Entry_Failed := True;
      end if;
   end Fail_Entry;

   -----------------
   -- First_Entry --
   -----------------

   function First_Entry
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Id);
   begin
      return Rose.Words.Word_64 (First_Committed);
   end First_Entry;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      if Page_Buffer_Last > 0 then
         --  Rose.Console_IO.Put ("log: writing ");
         --  Rose.Console_IO.Put (Natural (Page_Buffer_Last));
         --  Rose.Console_IO.Put (" page");
         --  if Page_Buffer_Last /= 1 then
         --     Rose.Console_IO.Put ("s");
         --  end if;
         --  Rose.Console_IO.Put (": location ");
         --  Rose.Console_IO.Put (Natural (Page_Buffer_Loc));
         --  Rose.Console_IO.New_Line;

         declare
            Success : Boolean;
         begin
            Operate
              (Location   => Page_Buffer_Loc,
               Addr       => Page_Buffer'Address,
               Operation  => Write,
               Page_Count => Natural (Page_Buffer_Last),
               Success    => Success);
            Page_Buffer_Last := 0;
            if not Success then
               Fail_Entry ("log: cannot write entry pages");
            end if;
         end;

         --  Rose.Console_IO.Put_Line ("done");

      end if;
   end Flush;

   ----------------------
   -- Get_Device_Index --
   ----------------------

   function Get_Device_Index
     (Location : Log_Location_Id)
      return Log_Device_Count
   is
   begin
      for Index in 1 .. Last_Log_Device loop
         if Location <= Log_Devices (Index).Last_Location then
            return Index;
         end if;
      end loop;
      Rose.Console_IO.Put ("log: no device for location ");
      Rose.Console_IO.Put (Natural (Location));
      Rose.Console_IO.New_Line;

      return 0;
   end Get_Device_Index;

   ------------------
   -- Is_Log_Entry --
   ------------------

   function Is_Log_Entry
     (Location : Log_Location_Id;
      Entry_Header : Log.Header.Entry_Header_Type)
      return Boolean
   is
      use type Rose.Words.Word_32;
      use type Rose.Words.Word_64;
   begin
      return Entry_Header.Magic = Log.Header.Entry_Magic
        and then Entry_Header.Version = Log_Version
        and then Entry_Header.Generation = Header.Generation
        and then Entry_Header.Log_First = Location + 1
        and then Entry_Header.Log_Last >= Location;
   end Is_Log_Entry;

   ----------------
   -- Last_Entry --
   ----------------

   function Last_Entry
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Id);
   begin
      return Rose.Words.Word_64 (Last_Committed);
   end Last_Entry;

   ----------------
   -- Load_Entry --
   ----------------

   function Load_Entry (Location : Log_Location_Id) return Boolean is
      Success : Boolean;
   begin
      if Location = 0
        or else Location < First_Committed
        or else Location > Last_Committed
      then
         return False;
      end if;

      if Location = Cached_Location then
         return True;
      end if;

      Cached_Location := 0;
      Read (Location, Cached_Header'Address, Success);
      if Success and then Is_Log_Entry (Location, Cached_Header) then
         Cached_Location := Location;
      end if;

      return Cached_Location /= 0;
   end Load_Entry;

   ----------------
   -- Next_Entry --
   ----------------

   function Next_Entry
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Id);
      Current : constant Log_Location_Id := Log_Location_Id (Location);
   begin
      if Current = Last_Committed or else not Load_Entry (Current) then
         return 0;
      end if;

      declare
         Next : constant Log_Location_Id := Cached_Header.Log_Last + 1;
      begin
         return (if Load_Entry (Next) then Rose.Words.Word_64 (Next) else 0);
      end;
   end Next_Entry;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Location   : Log_Location_Id;
      Addr       : System.Address;
      Operation  : Device_Operation;
      Page_Count : Natural;
      Success    : out Boolean)
   is
      Device_Index : constant Log_Device_Count :=
                       Get_Device_Index (Location);
   begin
      Success := False;

      if Device_Index = 0
        or else Location + Log_Location_Id (Page_Count) - 1
          > Log_Devices (Device_Index).Last_Location
      then
         return;
      end if;

      declare
         use System.Storage_Elements;

         Device  : Log_Device_Record renames Log_Devices (Device_Index);
         Count   : constant Natural := Device.Multiplier * Page_Count;
         Start   : constant Block_Address_Type :=
           Block_Address_Type (Location - Device.First_Location)
           * Block_Address_Type (Device.Multiplier);
         Data_Last : constant Storage_Count :=
           Log_Page_Size * Storage_Count (Page_Count);
         Data    : System.Storage_Elements.Storage_Array (1 .. Data_Last);
         pragma Import (Ada, Data);
         for Data'Address use Addr;
      begin
         case Operation is
            when Read =>
               Rose.Interfaces.Block_Device.Client.Read_Blocks
                 (Item   => Device.Client,
                  Start  => Start,
                  Count  => Count,
                  Blocks => Data);
            when Write =>
               --  Rose.Console_IO.Put ("Write_Blocks: start=");
               --  Rose.Console_IO.Put (Natural (Start));
               --  Rose.Console_IO.Put ("; count=");
               --  Rose.Console_IO.Put (Count);
               --  Rose.Console_IO.Put ("; bytes=");
               --  Rose.Console_IO.Put (Natural (Data_Last));
               --  Rose.Console_IO.New_Line;

               Rose.Interfaces.Block_Device.Client.Write_Blocks
                 (Item   => Device.Client,
                  Start  => Start,
                  Count  => Count,
                  Blocks => Data);

               --  Rose.Console_IO.Put_Line ("finished writing");

         end case;
      end;

      Success := True;
   end Operate;

   ----------------
   -- Page_Count --
   ----------------

   function Page_Count
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64)
      return Natural
   is
      pragma Unreferenced (Id);
      Item : constant Log_Location_Id := Log_Location_Id (Location);
   begin
      if Load_Entry (Item) then
         return Natural (Cached_Header.Log_Last - Item);
      else
         return 0;
      end if;
   end Page_Count;

   ----------
   -- Read --
   ----------

   procedure Read
     (Location : Log_Location_Id;
      Addr     : System.Address;
      Success  : out Boolean)
   is
   begin
      Operate (Location, Addr, Read, 1, Success);
   end Read;

   ---------------
   -- Read_Page --
   ---------------

   procedure Read_Page
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64;
      Index    : Positive;
      Page     : out System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      Item    : constant Log_Location_Id := Log_Location_Id (Location);
      Success : Boolean := False;
   begin
      if Page'Length /= Log_Page_Size then
         Rose.Console_IO.Put_Line ("log: read: buffer is not one page");
      elsif not Load_Entry (Item) then
         Rose.Console_IO.Put_Line ("log: read: not an entry");
      elsif Log_Location_Id (Index) > Cached_Header.Log_Last - Item then
         Rose.Console_IO.Put_Line ("log: read: no such page in entry");
      else
         Read (Item + Log_Location_Id (Index), Page'Address, Success);
      end if;

      if not Success then
         Page := (others => 0);
      end if;
   end Read_Page;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Id          : in     Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
      Data   : System.Storage_Elements.Storage_Array (1 .. 4096);
      pragma Import (Ada, Data);
      for Data'Address use Header'Address;
   begin
      if Last_Log_Device = 0 then
         Rose.Console_IO.Put_Line ("log: reset: no log device");
      else
         declare
            use type Rose.Words.Word_64;
            Generation : constant Rose.Words.Word_64 :=
                           (if Header.Magic = Log.Header.Log_Magic
                            then Header.Generation + 1
                            else 1);
            Success    : Boolean;
         begin
            Header := (others => <>);
            Header.Generation := Generation;
            Write (0, Header'Address, Success);
            if not Success then
               Rose.Console_IO.Put_Line ("log: reset: cannot write header");
            end if;
            Next_Log_Location := 1;
            First_Committed := 0;
            Last_Committed := 0;
            Cached_Location := 0;
         end;
      end if;
   end Reset;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Log_Server);
   end Start_Server;

   -----------
   -- Write --
   -----------

   procedure Write
     (Location : Log_Location_Id;
      Addr     : System.Address;
      Success  : out Boolean)
   is
   begin
      Operate (Location, Addr, Write, 1, Success);
   end Write;

end Log.Server;
