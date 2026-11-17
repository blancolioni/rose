with Rose.Capabilities;
with Rose.Server;

with Rose.Interfaces.Commands.Lookup.Client;
with Rose.Interfaces.Processes.Launch.Client;
with Rose.Interfaces.Commands.Map.Server;

with Command.Maps;

package body Command.Server is

   --  One object serves Map, and Commands as its ancestor. A cap for
   --  Commands has only Commands' operations, so it finds commands but
   --  cannot add or remove them (#460).

   type Registry_Object is
     new Rose.Interfaces.Commands.Map.Server.Map_Interface with null record;

   overriding function Map_Cap
     (Item : in out Registry_Object)
      return Rose.Capabilities.Capability;

   overriding function Lookup_Cap
     (Item : in out Registry_Object)
      return Rose.Capabilities.Capability;

   overriding procedure Add
     (Item : in out Registry_Object;
      Name : String;
      Cap  : in out Rose.Interfaces.Processes.Launch.Launch_Object'Class);

   overriding procedure Remove
     (Item : in out Registry_Object;
      Name : String);

   overriding function Find
     (Item : in out Registry_Object;
      Name : String)
      return Rose.Interfaces.Processes.Launch.Launch_Object'Class;

   overriding function Get_Commands
     (Item : in out Registry_Object)
      return Rose.Interfaces.Commands.Lookup.Lookup_Object'Class;

   Context      : Rose.Server.Server_Context;
   The_Registry : aliased Registry_Object;

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Item : in out Registry_Object;
      Name : String;
      Cap  : in out Rose.Interfaces.Processes.Launch.Launch_Object'Class)
   is
      pragma Unreferenced (Item);
   begin
      Command.Maps.Insert (Name, Cap.Launch_Cap);
   end Add;

   ----------------
   -- Lookup_Cap --
   ----------------

   overriding function Lookup_Cap
     (Item : in out Registry_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Commands.Map.Server.Cap_Of
         (Context, Item, Rose.Interfaces.Commands.Map.Lookup_Interface));

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Commands.Map.Server
        .Publish (Context, The_Registry'Access);
   end Create_Server;

   ----------
   -- Find --
   ----------

   --  A name that isn't registered gives a client with the null cap, which
   --  petal and login check for.

   overriding function Find
     (Item : in out Registry_Object;
      Name : String)
      return Rose.Interfaces.Processes.Launch.Launch_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      return Rose.Interfaces.Processes.Launch.Client.From_Cap (Command.Maps
        .Find (Name));
   end Find;

   ------------------
   -- Get_Commands --
   ------------------

   overriding function Get_Commands
     (Item : in out Registry_Object)
      return Rose.Interfaces.Commands.Lookup.Lookup_Object'Class
   is
   begin
      return Rose.Interfaces.Commands.Lookup.Client.From_Cap (Item
        .Lookup_Cap);
   end Get_Commands;

   -------------
   -- Map_Cap --
   -------------

   overriding function Map_Cap
     (Item : in out Registry_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Commands.Map.Server.Cap_Of (Context, Item));

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Item : in out Registry_Object;
      Name : String)
   is
      pragma Unreferenced (Item);
   begin
      Command.Maps.Delete (Name);
   end Remove;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Command.Server;
