--  Root package for Mem server

with Rose.Capabilities;

package Mem is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Take_Next_Cap       : constant Rose.Capabilities.Capability := 2;

   Console_Cap         : Rose.Capabilities.Capability;
   Region_Count_Cap    : Rose.Capabilities.Capability;
   Region_Range_Cap    : Rose.Capabilities.Capability;
   Start_Paging_Cap    : Rose.Capabilities.Capability;

end Mem;
