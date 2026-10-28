--  The capabilities the kernel gives init, and the indices they have in
--  init's capability table (#115).  The kernel installs them when it
--  creates init, in this order, so that init never has to make a cap
--  from a layout.

package Rose.Init_Capabilities is

   pragma Pure (Rose.Init_Capabilities);

   Create_Cap             : constant := 1;
   Boot_Cap               : constant := 2;
   Reserve_Memory_Cap     : constant := 3;
   Region_Count_Cap       : constant := 4;
   Get_Region_Cap         : constant := 5;
   Create_Endpoint_Cap    : constant := 6;
   Rescind_Cap            : constant := 7;
   Delete_Cap             : constant := 8;
   Exit_Cap               : constant := 9;
   Page_On_Cap            : constant := 10;
   Write_System_Image_Cap : constant := 11;
   Create_Process_Cap     : constant := 12;
   Set_Timeout_Cap        : constant := 13;
   Get_Ticks_Cap          : constant := 14;
   Enter_Checkpoint_Cap   : constant := 15;
   Leave_Checkpoint_Cap   : constant := 16;
   Start_Log_Cap          : constant := 17;
   Trace_Cap              : constant := 18;
   Restore_Process_Cap    : constant := 19;
   Resume_Process_Cap     : constant := 20;
   Restore_Interrupt_Cap  : constant := 21;

   --  The root caps.  Each returns a cap with less authority than its
   --  own, and none of the caps they return can do the same.

   Ports_Cap              : constant := 22;
   --  Data: the endpoint (one of the port endpoints below), the size
   --  (one of the port sizes below), and the first and last port, which
   --  are in 0 .. 16#FFFF#.  Replies with a port cap.

   Interrupts_Cap         : constant := 23;
   --  Data: an interrupt vector in First_Device_Vector ..
   --  Last_Device_Vector.  Replies with a cap that installs a handler for
   --  it.

   Device_Pages_Cap       : constant := 24;
   --  Data: a physical page number.  Replies with a cap that maps that
   --  page at the same address, if the page is not RAM.

   Last_Cap               : constant := 24;

   Derive_Ports_Endpoint     : constant := 5;
   Derive_Interrupt_Endpoint : constant := 2;
   Device_Page_Endpoint      : constant := 4;

   Port_Out_Endpoint       : constant := 1;
   Port_In_Endpoint        : constant := 2;
   Port_Out_Range_Endpoint : constant := 3;
   Port_In_Range_Endpoint  : constant := 4;

   Port_Size_8  : constant := 0;
   Port_Size_16 : constant := 1;
   Port_Size_32 : constant := 2;

   First_Device_Vector : constant := 32;
   Last_Device_Vector  : constant := 47;

end Rose.Init_Capabilities;
