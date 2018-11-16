with Rose.Addresses;                    use Rose.Addresses;
with Rose.Objects;

package Rose.Kernel is

   Max_Modules       : constant := 8;

   Max_Processes         : constant := 64;
   Max_Virtual_Pages     : constant := 2_000_000;

   Kernel_Object_Id    : constant Rose.Objects.Object_Id :=
                           16#0000_0000_0000_0001#;

   type Process_Segment is (Text, Static, Data, Environment);

   Environment_Virtual_Page    : constant := 16#000F_F000#;
   Environment_Virtual_Address : constant :=
     Environment_Virtual_Page * Rose.Addresses.Memory_Page_Size;

   type Message_Priority is mod 4;   --  0 is the highest

   function Kernel_Physical_Base return Rose.Addresses.Physical_Address;
   function Kernel_Physical_Bound return Rose.Addresses.Physical_Address;

private

   Kernel_Base, Kernel_Bound : Rose.Addresses.Virtual_Address;

   pragma Export (C, Kernel_Base, "_kmem_base");
   pragma Export (C, Kernel_Bound, "_kmem_bound");

   Kernel_Virtual_Base : constant := 16#C000_0000#;
   Kernel_Virtual_Page_Base : constant :=
                                Kernel_Virtual_Base
                                  / Rose.Addresses.Physical_Page_Bytes;

   function Kernel_Physical_Base return Rose.Addresses.Physical_Address
   is (Physical_Address (Kernel_Base - Kernel_Virtual_Base));

   function Kernel_Physical_Bound return Rose.Addresses.Physical_Address
   is (Physical_Address (Kernel_Bound - Kernel_Virtual_Base));

   function To_Kernel_Virtual_Address
     (Phys_Addr : Physical_Address)
     return Virtual_Address
   is (Virtual_Address (Phys_Addr) + Kernel_Virtual_Base);

   function To_Kernel_Physical_Address
     (Virtual_Addr : Virtual_Address)
     return Physical_Address
   is (Physical_Address (Virtual_Addr - Kernel_Virtual_Base));

   System_Call_Entry        : Physical_Address;
   pragma Import (C, System_Call_Entry, "system_call_entry");
   Log_Invocation           : Boolean := False;
   Log_Reply                : Boolean := False;
   Log_Port_IO              : Boolean := True;
   Log_Process_Activity     : Rose.Objects.Process_Id := 0;
   Log_Detailed_Invocation  : Rose.Objects.Process_Id := 0;
   Log_Process_Stack        : Rose.Objects.Process_Id := 0;
   Use_Serial_Port          : Boolean := True;

end Rose.Kernel;
