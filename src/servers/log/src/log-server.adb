with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Invocation;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Kernel_Log.Server;
with Rose.Interfaces.State.Server;

with Rose.Server;
with Rose.System_Calls.Client;

with Log.Header;

package body Log.Server is

   use Rose.Interfaces.Block_Device;
   subtype Block_Device_Client is
     Rose.Interfaces.Block_Device.Client.Block_Device_Client;

   Max_Log_Devices : constant := 16;

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

   Current_Log_Header : Log.Header.Entry_Header_Type;
   Header             : Log.Header.Log_Header_Type;
   Next_Log_Location  : Log_Location_Id;

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

   type Device_Operation is (Read, Write);

   procedure Operate
     (Location  : Log_Location_Id;
      Addr      : System.Address;
      Operation : Device_Operation);

   procedure Read
     (Location : Log_Location_Id;
      Addr     : System.Address);

   procedure Write
     (Location : Log_Location_Id;
      Addr     : System.Address);

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
            begin
               Read (0, Header'Address);
               if Header.Magic = Log.Header.Log_Magic then
                  Rose.Console_IO.Put_Line ("log: found valid log device");

                  Next_Log_Location := 1;
                  loop
                     Read (Next_Log_Location, Current_Log_Header'Address);
                     if Current_Log_Header.Magic = Log.Header.Entry_Magic then
                        Rose.Console_IO.Put
                          ("log: skipping entry at location ");
                        Rose.Console_IO.Put (Natural (Next_Log_Location));
                        Rose.Console_IO.New_Line;

                        Next_Log_Location := Current_Log_Header.Log_Last + 1;
                     else
                        exit;
                     end if;
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

      Current_Log_Header.Log_Last :=
        Current_Log_Header.Log_Last + 1;

      Write (Current_Log_Header.Log_Last, Data'Address);

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

      Write (Next_Log_Location, Current_Log_Header'Address);
      Next_Log_Location := Current_Log_Header.Log_Last + 1;
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
      Current_Log_Header.Flags := Log.Header.Is_Active_Entry;
      Current_Log_Header.Log_First := Next_Log_Location + 1;
      Current_Log_Header.Log_Last := Next_Log_Location;

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

   end Create_Server;

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

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Location  : Log_Location_Id;
      Addr      : System.Address;
      Operation : Device_Operation)
   is
      Device_Index : constant Log_Device_Count :=
                       Get_Device_Index (Location);
   begin
      if Device_Index = 0 then
         return;
      end if;

      declare
         Device  : Log_Device_Record renames Log_Devices (Device_Index);
         Count   : constant Natural := Device.Multiplier;
         Start   : constant Block_Address_Type :=
                     Block_Address_Type (Location - Device.First_Location)
                     * Block_Address_Type (Count);
         Data    : System.Storage_Elements.Storage_Array (1 .. Log_Page_Size);
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
               Rose.Interfaces.Block_Device.Client.Write_Blocks
                 (Item   => Device.Client,
                  Start  => Start,
                  Count  => Count,
                  Blocks => Data);
         end case;
      end;
   end Operate;

   ----------
   -- Read --
   ----------

   procedure Read
     (Location : Log_Location_Id;
      Addr     : System.Address)
   is
   begin
      Operate (Location, Addr, Read);
   end Read;

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
         Header := (others => <>);
         Write (0, Header'Address);
         Next_Log_Location := 1;
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
      Addr     : System.Address)
   is
   begin
      Operate (Location, Addr, Write);
   end Write;

end Log.Server;
