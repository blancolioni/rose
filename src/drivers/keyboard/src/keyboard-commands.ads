package Keyboard.Commands is

   KB_ACK      : constant := 16#FA#;
   KB_AUX_BYTE : constant := 16#20#;
   KB_OUT_FULL : constant := 16#01#;
   KB_IN_FULL  : constant := 16#02#;

   Read_Controller  : constant := 16#20#;
   Write_Controller : constant := 16#60#;
   Self_Test        : constant := 16#AA#;
   Disable_Aux      : constant := 16#A7#;
   Enable_Aux       : constant := 16#A8#;
   Disable_Keyboard : constant := 16#AD#;
   Enable_Keyboard  : constant := 16#AE#;
   Set_LED          : constant := 16#ED#;


end Keyboard.Commands;
