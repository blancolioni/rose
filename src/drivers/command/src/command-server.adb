with Rose.Objects;

with Rose.Server;
with Rose.Interfaces.Map.Server;

with Command.Maps;

package body Command.Server is

   Context : Rose.Server.Server_Context;

   Commands_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   --  The cap for the registry's Commands interface (#460), made the
   --  first time it is asked for.

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

   function Get_Commands_Handler
     (Id : Rose.Objects.Capability_Identifier)
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
      Command.Maps.Insert (Name, Cap);
   end Add_Handler;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Map.Server.Publish_Interface
        (Server_Context => Context,
         Add            => Add_Handler'Access,
         Remove         => Remove_Handler'Access,
         Find           => Find_Handler'Access,
         Get_Commands   => Get_Commands_Handler'Access);
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
      return Cap : constant Rose.Capabilities.Capability :=
        Maps.Find (Name);
      --  do
      --     Rose.Console_IO.Put ("command: returning cap ");
      --     Rose.Console_IO.Put (Natural (Cap));
      --     Rose.Console_IO.New_Line;
      --  end return;
   end Find_Handler;

   --------------------------
   -- Get_Commands_Handler --
   --------------------------

   function Get_Commands_Handler
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      use type Rose.Capabilities.Capability;
   begin
      --  The Map server answers the Commands interface endpoint with the
      --  Find cap alone, so a cap for it finds commands, but cannot add
      --  or remove them.

      if Commands_Cap = Rose.Capabilities.Null_Capability then
         Commands_Cap :=
           Rose.Server.Create_Endpoint
             (Rose.Interfaces.Map.Commands_Interface);
      end if;
      return Commands_Cap;
   end Get_Commands_Handler;

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
