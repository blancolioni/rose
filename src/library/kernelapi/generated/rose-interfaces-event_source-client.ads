with Rose.Interfaces.Event_Listener.Client;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Event_Source.Client is

   type Event_Source_Client is private;

   procedure Open_Cap_Set
     (Client       :    out Event_Source_Client;
      Add_Listener : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Event_Source_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Add_Listener
     (Item     : Event_Source_Client;
      Listener :
      Rose.Interfaces.Event_Listener.Client.Event_Listener_Client);

   function Get_Interface_Cap (Item : Event_Source_Client)
      return Rose.Capabilities.Capability;

   function Get_Add_Listener_Cap (Item : Event_Source_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Event_Source_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Add_Listener  : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Event_Source.Client;
