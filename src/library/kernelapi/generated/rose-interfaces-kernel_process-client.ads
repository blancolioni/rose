with Rose.Objects;
with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Kernel_Process.Client is

   type Kernel_Process_Client is private;

   procedure Open_Cap_Set
     (Client        :    out Kernel_Process_Client;
      Destroy       : in     Rose.Capabilities.Capability;
      Get_Object_Id : in     Rose.Capabilities.Capability;
      Resume        : in     Rose.Capabilities.Capability;
      Fault         : in     Rose.Capabilities.Capability;
      Notify        : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Kernel_Process_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Destroy (Item : Kernel_Process_Client);

   function Get_Object_Id (Item : Kernel_Process_Client)
      return Rose.Objects.Object_Id;

   procedure Resume (Item : Kernel_Process_Client);

   procedure Fault (Item : Kernel_Process_Client);

   procedure Notify
     (Item  : Kernel_Process_Client;
      Flags : Rose.Words.Word_32);

   function Get_Interface_Cap (Item : Kernel_Process_Client)
      return Rose.Capabilities.Capability;

   function Get_Resume_Cap (Item : Kernel_Process_Client)
      return Rose.Capabilities.Capability;

   function Get_Fault_Cap (Item : Kernel_Process_Client)
      return Rose.Capabilities.Capability;

   function Get_Notify_Cap (Item : Kernel_Process_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Kernel_Process_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Destroy       : Rose.Capabilities.Capability := 0;
         Get_Object_Id : Rose.Capabilities.Capability := 0;
         Resume        : Rose.Capabilities.Capability := 0;
         Fault         : Rose.Capabilities.Capability := 0;
         Notify        : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Kernel_Process.Client;
