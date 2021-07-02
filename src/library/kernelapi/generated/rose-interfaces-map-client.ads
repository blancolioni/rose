with Rose.Interfaces.Launch.Client;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Map.Client is

   type Map_Client is private;

   procedure Open_Cap_Set
     (Client :    out Map_Client;
      Add    : in     Rose.Capabilities.Capability;
      Remove : in     Rose.Capabilities.Capability;
      Find   : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Map_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Add
     (Item : Map_Client;
      Name : String;
      Cap  : Rose.Interfaces.Launch.Client.Launch_Client);

   procedure Remove
     (Item : Map_Client;
      Name : String);

   function Find
     (Item : Map_Client;
      Name : String)
   return Rose.Interfaces.Launch.Client.Launch_Client;

   function Get_Interface_Cap (Item : Map_Client)
      return Rose.Capabilities.Capability;

   function Get_Add_Cap (Item : Map_Client)
      return Rose.Capabilities.Capability;

   function Get_Remove_Cap (Item : Map_Client)
      return Rose.Capabilities.Capability;

   function Get_Find_Cap (Item : Map_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Map_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Add           : Rose.Capabilities.Capability := 0;
         Remove        : Rose.Capabilities.Capability := 0;
         Find          : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Map.Client;
