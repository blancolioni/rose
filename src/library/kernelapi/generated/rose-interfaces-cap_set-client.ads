with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Cap_Set.Client is

   type Cap_Set_Client is private;

   procedure Open_Cap_Set
     (Client  :    out Cap_Set_Client;
      Append  : in     Rose.Capabilities.Capability;
      Get_Cap : in     Rose.Capabilities.Capability;
      Length  : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Cap_Set_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Append
     (Item : Cap_Set_Client;
      Caps : Rose.Capabilities.Capability_Array);

   function Get_Cap
     (Item  : Cap_Set_Client;
      Index : Positive)
   return Rose.Capabilities.Capability;

   function Length (Item : Cap_Set_Client) return Natural;

   function Get_Interface_Cap (Item : Cap_Set_Client)
      return Rose.Capabilities.Capability;

   function Get_Append_Cap (Item : Cap_Set_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Cap_Cap (Item : Cap_Set_Client)
      return Rose.Capabilities.Capability;

   function Get_Length_Cap (Item : Cap_Set_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Cap_Set_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Append        : Rose.Capabilities.Capability := 0;
         Get_Cap       : Rose.Capabilities.Capability := 0;
         Length        : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Cap_Set.Client;
