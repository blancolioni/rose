with Rose.Capabilities;

package Restore is

   pragma Pure (Restore);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;
   Active_Swap_Cap     : constant Rose.Capabilities.Capability := 3;
   Inactive_Swap_Cap   : constant Rose.Capabilities.Capability := 4;
   Log_Cap             : constant Rose.Capabilities.Capability := 5;
   Add_Storage_Cap     : constant Rose.Capabilities.Capability := 6;
   Install_Media_Cap   : constant Rose.Capabilities.Capability := 7;

end Restore;
