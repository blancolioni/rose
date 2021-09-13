with Rose.Server;
with Rose.Capabilities;
with Rose.Words;
with System.Storage_Elements;
with Rose.Objects;
with Rose.Interfaces.Memory;

package Rose.Interfaces.Memory.Server is

   function Get_New_Process_Cap return Rose.Capabilities.Capability;
   function Get_Register_Process_Cap return Rose.Capabilities.Capability;
   function Get_Page_Fault_Cap return Rose.Capabilities.Capability;
   function Get_Take_Physical_Memory_Cap return Rose.Capabilities.Capability;

   type New_Process_Handler is access
     function
       (Id      : Rose.Objects.Capability_Identifier;
        Process : Rose.Capabilities.Capability)
     return Rose.Capabilities.Capability;

   type Register_Process_Handler is access
     procedure
       (Id          : Rose.Objects.Capability_Identifier;
        Process     : Rose.Capabilities.Capability;
        Exec_Base   : Rose.Words.Word;
        Exec_Bound  : Rose.Words.Word;
        Text_Base   : Rose.Words.Word;
        Text_Bound  : Rose.Words.Word;
        Data_Base   : Rose.Words.Word;
        Data_Bound  : Rose.Words.Word;
        Stack_Base  : Rose.Words.Word;
        Stack_Bound : Rose.Words.Word;
        Environment : System.Storage_Elements.Storage_Array);

   type Page_Fault_Handler is access
     procedure
       (Id              : Rose.Objects.Capability_Identifier;
        Faulting_Object : Rose.Objects.Object_Id;
        Virtual_Page    : Rose.Words.Word;
        Physical_Page   : Rose.Words.Word;
        Action          : Rose.Interfaces.Memory.Page_Access_Type);

   type Take_Physical_Memory_Handler is access
     procedure
       (Id     : in     Rose.Objects.Capability_Identifier;
        Size   : in     Rose.Words.Word;
        Start  :    out Rose.Words.Word;
        Amount :    out Rose.Words.Word);

   procedure Create_Server
     (Server_Context       : in out Rose.Server.Server_Context;
      New_Process          : in     New_Process_Handler;
      Register_Process     : in     Register_Process_Handler;
      Page_Fault           : in     Page_Fault_Handler;
      Take_Physical_Memory : in     Take_Physical_Memory_Handler;
      Instanced            : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context       : in out Rose.Server.Server_Context;
      New_Process          : in     New_Process_Handler;
      Register_Process     : in     Register_Process_Handler;
      Page_Fault           : in     Page_Fault_Handler;
      Take_Physical_Memory : in     Take_Physical_Memory_Handler;
      Instanced            : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context       : in out Rose.Server.Server_Context;
      New_Process          : in     New_Process_Handler;
      Register_Process     : in     Register_Process_Handler;
      Page_Fault           : in     Page_Fault_Handler;
      Take_Physical_Memory : in     Take_Physical_Memory_Handler);

private

end Rose.Interfaces.Memory.Server;
