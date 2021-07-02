with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Heap.Client is

   type Heap_Client is private;

   procedure Open_Cap_Set
     (Client            :    out Heap_Client;
      Current_Bound     : in     Rose.Capabilities.Capability;
      Request_New_Bound : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Heap_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Current_Bound (Item : Heap_Client) return Rose.Words.Word;

   procedure Request_New_Bound
     (Item      : Heap_Client;
      New_Bound : Rose.Words.Word);

   function Get_Interface_Cap (Item : Heap_Client)
      return Rose.Capabilities.Capability;

   function Get_Current_Bound_Cap (Item : Heap_Client)
      return Rose.Capabilities.Capability;

   function Get_Request_New_Bound_Cap (Item : Heap_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Heap_Client is
      record
         Is_Open           : Boolean := False;
         Interface_Cap     : Rose.Capabilities.Capability := 0;
         Current_Bound     : Rose.Capabilities.Capability := 0;
         Request_New_Bound : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Heap.Client;
