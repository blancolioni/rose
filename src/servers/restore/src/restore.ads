with Rose.Capabilities;

package Restore is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Take_Next_Cap       : constant Rose.Capabilities.Capability := 2;

   Delete_Endpoint_Cap : Rose.Capabilities.Capability;
   Console_Cap         : Rose.Capabilities.Capability;
   Active_Swap_Cap     : Rose.Capabilities.Capability;
   Inactive_Swap_Cap   : Rose.Capabilities.Capability;
   Log_Cap             : Rose.Capabilities.Capability;
   Add_Storage_Cap     : Rose.Capabilities.Capability;
   Reserve_Storage_Cap : Rose.Capabilities.Capability;
   Write_System_Image  : Rose.Capabilities.Capability;
   Install_Media_Cap   : Rose.Capabilities.Capability;
   Install_Exec_Cap    : Rose.Capabilities.Capability;

end Restore;
