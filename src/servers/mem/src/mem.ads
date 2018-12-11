--  Root package for Mem server

with Rose.Capabilities;

package Mem is

   pragma Pure (Mem);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;
   Region_Count_Cap    : constant Rose.Capabilities.Capability := 3;
   Region_Range_Cap    : constant Rose.Capabilities.Capability := 4;
   Start_Paging_Cap    : constant Rose.Capabilities.Capability := 5;

end Mem;
