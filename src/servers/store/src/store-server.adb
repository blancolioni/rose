with System.Storage_Elements;

with Rose.Words;

with Rose.Console_IO;
with Rose.Trace;
with Rose.System_Calls.Client;

with Rose.Interfaces.Storage.Store.Server;
with Rose.Interfaces.Storage.Bank.Server;
with Rose.Interfaces.Storage.Dependent.Server;
with Rose.Interfaces.Storage.Process_Banks.Server;
with Rose.Interfaces.Storage.Region.Server;
with Rose.Interfaces.Checkpoints.Storage_Checkpoint.Server;
with Rose.Interfaces.Streams.Reader.Server;

with Store.Devices;

package body Store.Server is

   procedure Map_Bss;
   --  Touch each page of the bss, so that mem maps it now (#914)

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));
   begin

      Delete_Endpoint_Cap := Get_Cap (1);
      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap,
         Delete_Cap      => Delete_Endpoint_Cap);

      Console_Cap := Get_Cap (2);
      Rose.Trace.Set_Trace_Cap (Get_Cap (3));

      --  Nothing reads the cap set again (#826).

      Rose.System_Calls.Delete_Cap (Get_Cap_From_Set);

      Rose.Console_IO.Open (Console_Cap);

      Rose.Interfaces.Storage.Store.Server.Create_Server
        (Server_Context    => Server_Context,
         Add_Backing_Store => Store.Devices.Add_Backing_Store'Access,
         Get_Root_Bank     => Store.Devices.Get_Root_Bank'Access);
      Rose.Interfaces.Storage.Bank.Server.Attach_Interface
        (Server_Context,
         Store.Devices.Reserve_From_Bank'Access,
         Store.Devices.Create_Sub_Bank'Access,
         Store.Devices.Add_Dependent'Access,
         Store.Devices.Get_Usage'Access,
         Store.Devices.Destroy_Bank'Access,
         Store.Devices.Bank_Index'Access,
         Instanced => True);
      Rose.Interfaces.Storage.Process_Banks.Server.Attach_Interface
        (Server_Context,
         Create_Process_Bank => Store.Devices.Create_Process_Bank'Access,
         Process_Bank        => Store.Devices.Process_Bank'Access,
         End_Process         => Store.Devices.End_Process'Access,
         Set_Ended           => Store.Devices.Set_Ended'Access,
         Take_Ended          => Store.Devices.Take_Ended'Access,
         Hand_Over           => Store.Devices.Hand_Over'Access,
         Parent_Bank         => Store.Devices.Parent_Bank'Access);
      Rose.Interfaces.Storage.Dependent.Server.Attach_Interface
        (Server_Context,
         Release   => Store.Devices.Release_Dependent'Access,
         Destroyed => Store.Devices.Dependent_Destroyed'Access,
         Instanced => True);
      Rose.Interfaces.Storage.Region.Server.Attach_Interface
        (Server_Context   => Server_Context,
         Get_Range        => Store.Devices.Get_Range'Access,
         Get              => Store.Devices.Get'Access,
         Put              => Store.Devices.Put'Access,
         Read             => Store.Devices.Read_Stream'Access,
         Release          => Store.Devices.Release'Access,
         Create_Subregion => Store.Devices.Create_Subregion'Access,
         Instanced        => True);

      --  The interface caps of a released region or bank make no caps
      --  for it: they would never be removed (#967).

      Rose.Interfaces.Storage.Bank.Server.Set_Owns
        (Store.Devices.Owns_Bank'Access);
      Rose.Interfaces.Storage.Region.Server.Set_Owns
        (Store.Devices.Owns_Region'Access);

      Rose.Interfaces.Streams.Reader.Server.Attach_Interface
        (Server_Context => Server_Context,
         Read           => Store.Devices.Read'Access,
         Instanced      => True);

      Rose.Interfaces.Checkpoints.Storage_Checkpoint.Server.Attach_Interface
        (Server_Context     => Server_Context,
         Checkpoint         => Store.Devices.Checkpoint'Access,
         Committed          => Store.Devices.Committed'Access,
         Restore_Region     => Store.Devices.Restore_Region'Access,
         Restore_Table_Page => Store.Devices.Restore_Table_Page'Access,
         Find_Region        => Store.Devices.Find_Region'Access);
   end Create_Server;

   -------------
   -- Map_Bss --
   -------------

   procedure Map_Bss is
      use System.Storage_Elements;
      Bss_Start : constant Storage_Element
        with Import, Convention => C, External_Name => "__bss_start";
      Bss_End   : constant Storage_Element
        with Import, Convention => C, External_Name => "_end";
      Page_Size : constant := 4096;
      First     : constant Integer_Address := To_Integer (Bss_Start'Address);
      Last      : constant Integer_Address := To_Integer (Bss_End'Address);
      Address   : Integer_Address := First - First mod Page_Size;
   begin
      while Address < Last loop

         --  A write, not a read: mem maps a page read-only until it is
         --  written, so that it knows the page is dirty, and the first
         --  write faults again.

         declare
            Byte : Storage_Element
              with Import, Volatile, Address => To_Address (Address);
         begin
            Byte := Byte;
         end;
         Address := Address + Page_Size;
      end loop;
   end Map_Bss;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Console_IO.Put_Line ("storage: starting server");

      --  Mem waits in its calls to the store, and only mem serves the
      --  store's page faults, so the store must not fault in one of those
      --  calls (#914). Mem maps a page of the bss the first time it is
      --  touched, and makes it writable the first time it is written, so
      --  each is written now, while mem is free to serve the faults. The
      --  endpoints are registered already, so init isn't kept waiting for
      --  them.

      Map_Bss;
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end Store.Server;
