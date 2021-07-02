with Rose.Objects;

package Rose.Interfaces.Memory is

   type Page_Access_Type is (Read, Write, Execute);
   Memory_Interface : constant Rose.Objects.Endpoint_Id :=
      16#C027_8DBF_A2DF#;
   New_Process_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#C196_0CA8_7972#;
   Register_Process_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#533E_4DCC_E3A5#;
   Page_Fault_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#C6FF_984F_29F6#;
   Take_Physical_Memory_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#2363_36CC_C1A7#;

end Rose.Interfaces.Memory;
