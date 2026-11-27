--  Root package for Mem server

with Rose.Capabilities;

package Mem is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;

   Console_Cap         : Rose.Capabilities.Capability;
   Region_Count_Cap    : Rose.Capabilities.Capability;
   Region_Range_Cap    : Rose.Capabilities.Capability;
   Start_Paging_Cap    : Rose.Capabilities.Capability;

   Free_Caps_Cap       : Rose.Capabilities.Capability := 0;
   --  The kernel's count of the caps mem can still create (#1131).

   Add_Copy_Frames_Cap : Rose.Capabilities.Capability := 0;
   Copy_Saved_Page_Cap : Rose.Capabilities.Capability := 0;
   End_Page_Copies_Cap : Rose.Capabilities.Capability := 0;
   --  The kernel's copies of pages written during a checkpoint (ADR
   --  0030): mem gives it frames, saves pages through it, and ends the
   --  copying when it has saved them.

   type Process_Id is mod 2 ** 16;
   Null_Process_Id : constant Process_Id := 0;

end Mem;
