with System.Standard_Caps;

package body System.Caps is

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return Rose.Capabilities.Capability is
   begin
      return System.Standard_Caps.Standard_Error_Cap;
   end Standard_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return Rose.Capabilities.Capability is
   begin
      return System.Standard_Caps.Standard_Input_Cap;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return Rose.Capabilities.Capability is
   begin
      return System.Standard_Caps.Standard_Output_Cap;
   end Standard_Output;

end System.Caps;
