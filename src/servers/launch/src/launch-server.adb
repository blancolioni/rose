with Rose.Invocation;
with Rose.Objects;

with Rose.Console_IO;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Launch.Server;
with Rose.Interfaces.Storage.Client;

package body Launch.Server is

   procedure On_Launch
     (Id   : in     Rose.Objects.Capability_Identifier;
      Caps : Rose.Capabilities.Capability_Array);

   Context : Rose.Server.Server_Context;
   Storage : Rose.Interfaces.Storage.Client.Storage_Client;
   Base_Object_Id : Rose.Objects.Object_Id;
   Bound_Object_Id : Rose.Objects.Object_Id;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Console_IO.Put_Line ("launch: creating server");
      Rose.Interfaces.Launch.Server.Create_Server
        (Context, On_Launch'Access);
      Rose.Interfaces.Storage.Client.Open (Storage, Storage_Cap);
   end Create_Server;

   ---------------
   -- On_Launch --
   ---------------

   procedure On_Launch
     (Id   : in     Rose.Objects.Capability_Identifier;
      Caps : Rose.Capabilities.Capability_Array)
   is
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Create_Process_Cap);
      Invoke_Capability (Params);
   end On_Launch;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Launch.Server;
