with System.Storage_Elements;

with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;

with Rose.Interfaces.Storage.Server;
with Rose.Server;

with Store.Devices;

package body Store.Server is

   procedure Reserve_Storage
     (Id    : in     Rose.Objects.Capability_Identifier;
      Size  : in     Rose.Words.Word_64;
      Base  :    out Rose.Objects.Object_Id;
      Bound :    out Rose.Objects.Object_Id);

   procedure Get
     (Id     : in     Rose.Objects.Capability_Identifier;
      Object : in     Rose.Objects.Object_Id;
      Data   :    out System.Storage_Elements.Storage_Array)
   is null;

   procedure Put
     (Id     : in     Rose.Objects.Capability_Identifier;
      Object : in     Rose.Objects.Object_Id;
      Data   : in     System.Storage_Elements.Storage_Array)
   is null;

   Server_Context : Rose.Server.Server_Context;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Storage.Server.Create_Server
        (Server_Context    => Server_Context,
         Reserve_Storage   => Reserve_Storage'Access,
         Add_Backing_Store => Store.Devices.Add_Backing_Store'Access,
         Get => Get'Access,
         Put => Put'Access);
   end Create_Server;

   ---------------------
   -- Reserve_Storage --
   ---------------------

   procedure Reserve_Storage
     (Id    : in     Rose.Objects.Capability_Identifier;
      Size  : in     Rose.Words.Word_64;
      Base  :    out Rose.Objects.Object_Id;
      Bound :    out Rose.Objects.Object_Id)
   is null;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Console_IO.Put_Line ("storage: starting server");
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end Store.Server;
