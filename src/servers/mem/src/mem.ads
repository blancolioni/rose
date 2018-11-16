--  Root package for Mem server

with Rose.Capabilities;
with Rose.Objects;

package Mem is

   pragma Pure (Mem);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;
   Region_Count_Cap    : constant Rose.Capabilities.Capability := 3;
   Region_Range_Cap    : constant Rose.Capabilities.Capability := 4;
   Start_Paging_Cap    : constant Rose.Capabilities.Capability := 5;

   Process_Launched_Endpoint : constant Rose.Objects.Endpoint_Id := 100;
   Process_Killed_Endpoint   : constant Rose.Objects.Endpoint_Id := 101;
   Page_Fault_Endpoint       : constant Rose.Objects.Endpoint_Id := 102;

   type Action_Type is (Read, Write, Execute);

end Mem;
