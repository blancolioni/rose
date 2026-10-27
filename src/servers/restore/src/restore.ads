with Rose.Capabilities;

package Restore is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;

   Delete_Endpoint_Cap : Rose.Capabilities.Capability;
   Console_Cap         : Rose.Capabilities.Capability;
   Swap_Cap            : Rose.Capabilities.Capability;
   Log_Cap             : Rose.Capabilities.Capability;
   Add_Storage_Cap     : Rose.Capabilities.Capability;
   Reserve_Storage_Cap : Rose.Capabilities.Capability;
   Write_System_Image  : Rose.Capabilities.Capability;
   Install_Media_Cap   : Rose.Capabilities.Capability;
   Install_Exec_Cap    : Rose.Capabilities.Capability;
   Add_Log_Device_Cap  : Rose.Capabilities.Capability;
   Log_Reader_Cap      : Rose.Capabilities.Capability;

   --  For restoring a checkpoint (Restore.Checkpoints).
   Restore_Process_Cap   : Rose.Capabilities.Capability;
   Resume_Process_Cap    : Rose.Capabilities.Capability;
   Restore_Interrupt_Cap : Rose.Capabilities.Capability;
   Store_Restore_Cap     : Rose.Capabilities.Capability;
   Memory_Restore_Cap    : Rose.Capabilities.Capability;
   Log_Reset_Cap         : Rose.Capabilities.Capability;

end Restore;
