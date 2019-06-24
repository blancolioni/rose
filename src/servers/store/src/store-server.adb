with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;
with Rose.System_Calls.Client;

with Rose.Interfaces.Storage.Server;
with Rose.Interfaces.Region.Server;
with Rose.Interfaces.Stream_Reader.Server;

with Store.Devices;

package body Store.Server is

   function Reserve_Storage
     (Id    : in     Rose.Objects.Capability_Identifier;
      Size  : in     Rose.Words.Word_64)
      return Rose.Capabilities.Capability;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin

      Delete_Endpoint_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Console_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Rose.Console_IO.Open (Console_Cap);

      Rose.Interfaces.Storage.Server.Create_Server
        (Server_Context    => Server_Context,
         Reserve_Storage   => Reserve_Storage'Access,
         Add_Backing_Store => Store.Devices.Add_Backing_Store'Access);
      Rose.Interfaces.Region.Server.Attach_Interface
        (Server_Context,
         Store.Devices.Get_Range'Access,
         Store.Devices.Get'Access,
         Store.Devices.Put'Access,
         Store.Devices.Read_Stream'Access,
         Store.Devices.Create_Subregion'Access,
         Instanced => True);

      Rose.Interfaces.Stream_Reader.Server.Attach_Interface
        (Server_Context => Server_Context,
         Read           => Store.Devices.Read'Access,
         Instanced      => True);
   end Create_Server;

   ---------------------
   -- Reserve_Storage --
   ---------------------

   function Reserve_Storage
     (Id    : in     Rose.Objects.Capability_Identifier;
      Size  : in     Rose.Words.Word_64)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Store.Devices.Reserve_Storage (Size);
   end Reserve_Storage;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Console_IO.Put_Line ("storage: starting server");
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end Store.Server;
