with Rose.Capabilities;

package Restore is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;

   --  The caps init gives the restore server, in this order (see
   --  Restore.Start.Get_Caps).

   Console_Cap         : Rose.Capabilities.Capability;
   Swap_Cap            : Rose.Capabilities.Capability;
   Log_Cap             : Rose.Capabilities.Capability;
   Add_Storage_Cap     : Rose.Capabilities.Capability;
   Init_Cap            : Rose.Capabilities.Capability;
   --  Init's install endpoint.  restore-install sends it each program to
   --  install, and both restore servers send it their last message.
   Add_Log_Device_Cap  : Rose.Capabilities.Capability;
   Log_Reader_Cap      : Rose.Capabilities.Capability;

   --  For restoring a checkpoint (Restore.Checkpoints).
   Restore_Process_Cap   : Rose.Capabilities.Capability;
   Resume_Process_Cap    : Rose.Capabilities.Capability;
   Restore_Interrupt_Cap : Rose.Capabilities.Capability;
   Store_Restore_Cap     : Rose.Capabilities.Capability;
   Memory_Restore_Cap    : Rose.Capabilities.Capability;

   --  Only restore-install has these (Restore.Install_Driver).
   Install_Media_Cap   : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Log_Reset_Cap       : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;

end Restore;
