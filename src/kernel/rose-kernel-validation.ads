with Rose.Capabilities.Layout;
with Rose.Objects;

package Rose.Kernel.Validation is

   procedure Create_Cap
     (Process : Rose.Objects.Process_Id;
      Cap     : Rose.Capabilities.Capability;
      Cap_Type : Rose.Capabilities.Layout.Capability_Type);

   procedure Delete_Cap
     (Process : Rose.Objects.Process_Id;
      Cap     : Rose.Capabilities.Capability);

   procedure Validate;

   procedure Save_Current_Process;
   pragma Export (C, Save_Current_Process, "validation_save_process");

   procedure Check_Current_Process;
   pragma Export (C, Check_Current_Process, "validation_check_process");

end Rose.Kernel.Validation;
