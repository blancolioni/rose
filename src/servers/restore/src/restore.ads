with Rose.Capabilities;

package Restore is

   pragma Pure (Restore);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Delete_Cap          : constant Rose.Capabilities.Capability := 2;
   Console_Cap         : constant Rose.Capabilities.Capability := 3;
   Active_Swap_Cap     : constant Rose.Capabilities.Capability := 4;
   Inactive_Swap_Cap   : constant Rose.Capabilities.Capability := 5;
   Log_Cap             : constant Rose.Capabilities.Capability := 6;
   Add_Storage_Cap     : constant Rose.Capabilities.Capability := 7;
   Write_System_Image  : constant Rose.Capabilities.Capability := 8;
   Install_Media_Cap   : constant Rose.Capabilities.Capability := 9;
   Install_Exec_Cap    : constant Rose.Capabilities.Capability := 10;

end Restore;
