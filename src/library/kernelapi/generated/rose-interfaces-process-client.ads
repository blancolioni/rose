with Rose.Words;
with Rose.Interfaces.Region.Client;
with Rose.Capabilities;
with Rose.Objects;
with Rose.Interfaces.Heap.Client;
with Rose.Invocation;

package Rose.Interfaces.Process.Client is

   type Process_Client is private;

   procedure Open_Cap_Set
     (Client                    :    out Process_Client;
      Add_Segment               : in     Rose.Capabilities.Capability;
      Add_Nonpersistent_Segment : in     Rose.Capabilities.Capability;
      Published_Interface       : in     Rose.Capabilities.Capability;
      Destroy                   : in     Rose.Capabilities.Capability;
      Get_Object_Id             : in     Rose.Capabilities.Capability;
      Heap_Interface            : in     Rose.Capabilities.Capability;
      Exit_Process              : in     Rose.Capabilities.Capability;
      Publish_Interface         : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Process_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Add_Segment
     (Item          : Process_Client;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Interfaces.Region.Client.Region_Client;
      Region_Offset : Rose.Words.Word;
      Flags         : Rose.Words.Word);

   procedure Add_Nonpersistent_Segment
     (Item          : Process_Client;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Flags         : Rose.Words.Word);

   function Published_Interface (Item : Process_Client)
      return Rose.Capabilities.Capability;

   procedure Destroy (Item : Process_Client);

   function Get_Object_Id (Item : Process_Client)
      return Rose.Objects.Object_Id;

   function Heap_Interface (Item : Process_Client)
      return Rose.Interfaces.Heap.Client.Heap_Client;

   procedure Exit_Process
     (Item        : Process_Client;
      Exit_Status : Natural);

   procedure Publish_Interface
     (Item          : Process_Client;
      Interface_Cap : Rose.Capabilities.Capability);

   function Get_Interface_Cap (Item : Process_Client)
      return Rose.Capabilities.Capability;

   function Get_Heap_Interface_Cap (Item : Process_Client)
      return Rose.Capabilities.Capability;

   function Get_Exit_Process_Cap (Item : Process_Client)
      return Rose.Capabilities.Capability;

   function Get_Publish_Interface_Cap (Item : Process_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Process_Client is
      record
         Is_Open                   : Boolean := False;
         Interface_Cap             : Rose.Capabilities.Capability := 0;
         Add_Segment               : Rose.Capabilities.Capability := 0;
         Add_Nonpersistent_Segment : Rose.Capabilities.Capability := 0;
         Published_Interface       : Rose.Capabilities.Capability := 0;
         Destroy                   : Rose.Capabilities.Capability := 0;
         Get_Object_Id             : Rose.Capabilities.Capability := 0;
         Heap_Interface            : Rose.Capabilities.Capability := 0;
         Exit_Process              : Rose.Capabilities.Capability := 0;
         Publish_Interface         : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Process.Client;
