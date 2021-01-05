with Rose.Objects;
with Rose.Console_IO;

with Rose.Server;
with Rose.Interfaces.Map.Server;

with Command.Maps;

package body Command.Server is

   Context : Rose.Server.Server_Context;

   procedure Add_Handler
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String;
      Cap  : Rose.Capabilities.Capability);

   procedure Remove_Handler
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String);

   function Find_Handler
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
        return Rose.Capabilities.Capability;

   -----------------
   -- Add_Handler --
   -----------------

   procedure Add_Handler
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String;
      Cap  : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
   begin
      Rose.Console_IO.Put ("command: saving cap ");
      Rose.Console_IO.Put (Natural (Cap));
      Rose.Console_IO.Put (" as ");
      Rose.Console_IO.Put (Name);
      Rose.Console_IO.New_Line;
      Command.Maps.Insert (Name, Cap);
   end Add_Handler;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Map.Server.Create_Server
        (Server_Context => Context,
         Add            => Add_Handler'Access,
         Remove         => Remove_Handler'Access,
         Find           => Find_Handler'Access);
   end Create_Server;

   ------------------
   -- Find_Handler --
   ------------------

   function Find_Handler
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Maps.Find (Name);
   end Find_Handler;

   --------------------
   -- Remove_Handler --
   --------------------

   procedure Remove_Handler
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
   is
      pragma Unreferenced (Id);
   begin
      Command.Maps.Delete (Name);
   end Remove_Handler;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Command.Server;
