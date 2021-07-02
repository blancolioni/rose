with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Event_Listener.Client is

   type Event_Listener_Client is private;

   procedure Open_Cap_Set
     (Client   :    out Event_Listener_Client;
      On_Event : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Event_Listener_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure On_Event
     (Item : Event_Listener_Client;
      Code : Rose.Words.Word);

   function Get_Interface_Cap (Item : Event_Listener_Client)
      return Rose.Capabilities.Capability;

   function Get_On_Event_Cap (Item : Event_Listener_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Event_Listener_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         On_Event      : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Event_Listener.Client;
