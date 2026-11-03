with Rose.Words;

package Rose.Kernel.Capabilities.Pages is

   use type Rose.Objects.Object_Id;

   Identity_Map_Page  : constant Rose.Objects.Endpoint_Index := 1;

   Map_Device_Range   : constant Rose.Objects.Endpoint_Index := 2;
   --  A range of device pages (#582).  Data word 0 is a page-aligned
   --  address in the device window (Device_Window_Base ..
   --  Device_Window_Bound), and the whole range is mapped there, writable
   --  and not executable, if it fits.  A page that is RAM is refused.

   Max_Device_Range_Pages : constant :=
                              (Device_Window_Bound - Device_Window_Base)
                              / Physical_Page_Bytes;

   function Device_Range_Payload
     (First_Page : Physical_Page_Address;
      Page_Count : Rose.Words.Word)
      return Rose.Objects.Object_Id
   is (Rose.Objects.Page_Object_Id'First
       + Rose.Objects.Object_Id (First_Page)
       + Rose.Objects.Object_Id (Page_Count) * 2 ** 32);
   --  The payload of a Map_Device_Range cap: the first page in the low
   --  32 bits, and the page count above it.

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Pages;
