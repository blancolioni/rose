with Rose.Addresses;                    use Rose.Addresses;
with Rose.Objects;

package Rose.Kernel is

   Max_Modules       : constant := 8;

   Max_Processes         : constant := 64;
   Max_Virtual_Pages     : constant := 2_000_000;

   Kernel_Object_Id    : constant Rose.Objects.Object_Id :=
                           16#0000_0000_0000_0001#;

   Process_Stack_Base            : constant := 16#D000_0000#;
   Process_Stack_Bound           : constant := 16#E000_0000#;
   Environment_Base              : constant := 16#E000_0000#;
   Environment_Bound             : constant := 16#E100_0000#;
   Invocation_Buffer_Range_Base  : constant := 16#EF00_0000#;
   Invocation_Buffer_Range_Bound : constant := 16#F000_0000#;

   function Kernel_Physical_Base return Rose.Addresses.Physical_Address;
   function Kernel_Physical_Bound return Rose.Addresses.Physical_Address;

   System_Image_Start_Magic : constant :=
                                16#e668_f3c2_10fd_3de7#;

   System_Image_End_Magic : constant :=
                                16#5557_b148_c5b3_f35b#;

private

   Kernel_Base, Kernel_Bound : Rose.Addresses.Virtual_Address;

   pragma Export (C, Kernel_Base, "_kmem_base");
   pragma Export (C, Kernel_Bound, "_kmem_bound");

   Kernel_Virtual_Base : constant := 16#F000_0000#;
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

   Checkpoint_Active    : Boolean := False;
   Image_Write_Active   : Boolean := False;
   Current_Image_Object : Rose.Objects.Object_Id :=
                            Rose.Objects.Null_Object_Id;
   Image_Writer         : Rose.Objects.Object_Id :=
                            Rose.Objects.Null_Object_Id;

   System_Call_Entry        : Physical_Address;
   pragma Import (C, System_Call_Entry, "system_call_entry");
   Log_Invocation           : Boolean := False;
   Log_Reply                : Boolean := False;
   Log_Port_IO              : Boolean := False;
   Use_Serial_Port          : Boolean := True;
   Log_Cap_Copy             : Boolean := False;
   Log_Endpoint_Cap_Create  : Boolean := False;

   Log_Object_Id : Rose.Objects.Object_Id := Rose.Objects.Null_Object_Id;

end Rose.Kernel;
