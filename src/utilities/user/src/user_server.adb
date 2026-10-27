with Rose.Capabilities.Standard;
with Rose.Objects;
with Rose.Server;

with Rose.Interfaces.Process.Client;
with Rose.Interfaces.User.Server;

package body User_Server is

   Context   : Rose.Server.Server_Context;
   Name      : String (1 .. Max_Name_Length);
   Name_Last : Natural := 0;
   Home_Cap  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   User_Cap  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   function Home_Directory
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure Get_Name
     (Id     : Rose.Objects.Capability_Identifier;
      Result : out String;
      Last   : out Natural);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (User_Name : String;
      Home      : Rose.Capabilities.Capability)
   is
      Self : Rose.Interfaces.Process.Client.Process_Client;
   begin
      Name_Last := User_Name'Length;
      Name (1 .. Name_Last) := User_Name;
      Home_Cap := Home;

      Rose.Interfaces.User.Server.Publish_Interface
        (Server_Context => Context,
         Name           => Get_Name'Access,
         Home_Directory => Home_Directory'Access);

      --  The generated server keeps the cap it publishes to itself, so
      --  ask the process for it.

      Rose.Interfaces.Process.Client.Open
        (Self, Rose.Capabilities.Standard.Process_Interface_Cap);
      User_Cap := Rose.Interfaces.Process.Client.Published_Interface (Self);
   end Create_Server;

   --------------
   -- Get_Name --
   --------------

   procedure Get_Name
     (Id     : Rose.Objects.Capability_Identifier;
      Result : out String;
      Last   : out Natural)
   is
      pragma Unreferenced (Id);
      Length : constant Natural := Natural'Min (Name_Last, Result'Length);
   begin
      Last := Result'First + Length - 1;
      Result (Result'First .. Last) := Name (1 .. Length);
   end Get_Name;

   --------------------
   -- Home_Directory --
   --------------------

   function Home_Directory
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Home_Cap;
   end Home_Directory;

   -------------------
   -- Interface_Cap --
   -------------------

   function Interface_Cap return Rose.Capabilities.Capability is
   begin
      return User_Cap;
   end Interface_Cap;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end User_Server;
