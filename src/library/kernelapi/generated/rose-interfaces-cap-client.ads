with Rose.Objects;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Cap.Client is

   type Cap_Client is private;

   procedure Open_Cap_Set
     (Client        :    out Cap_Client;
      Destroy       : in     Rose.Capabilities.Capability;
      Get_Object_Id : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Cap_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Destroy (Item : Cap_Client);

   function Get_Object_Id (Item : Cap_Client) return Rose.Objects.Object_Id;

   function Get_Interface_Cap (Item : Cap_Client)
      return Rose.Capabilities.Capability;

   function Get_Destroy_Cap (Item : Cap_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Object_Id_Cap (Item : Cap_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Cap_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Destroy       : Rose.Capabilities.Capability := 0;
         Get_Object_Id : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Cap.Client;
