with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;

with Rose.Interfaces.Storage.Server;
with Rose.Server;

package body Store.Server is

     procedure Reserve_Storage
       (Id    : in     Rose.Objects.Capability_Identifier;
        Size  : in     Rose.Words.Word_64;
        Base  :    out Rose.Objects.Object_Id;
        Bound :    out Rose.Objects.Object_Id);

     procedure Add_Backing_Store
       (Id    : in     Rose.Objects.Capability_Identifier;
        Store : in     Rose.Capabilities.Capability);

   Server_Context : Rose.Server.Server_Context;

   -----------------------
   -- Add_Backing_Store --
   -----------------------

   procedure Add_Backing_Store
     (Id    : in     Rose.Objects.Capability_Identifier;
      Store : in     Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
   begin
      Rose.Console_IO.Put ("storage: add backing store: ");
      Rose.Console_IO.Put (Natural (Store));
      Rose.Console_IO.New_Line;
   end Add_Backing_Store;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Storage.Server.Create_Server
        (Server_Context    => Server_Context,
         Reserve_Storage   => Reserve_Storage'Access,
         Add_Backing_Store => Add_Backing_Store'Access);
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
