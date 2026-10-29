with System.Storage_Elements;
with Rose.Words;

package Keyboard.Codes is

   procedure Handle_Key
     (Scan_Code : Rose.Words.Word_8;
      Result    : out System.Storage_Elements.Storage_Array;
      Last      : out System.Storage_Elements.Storage_Count);
   --  Scan_Code is a byte from the keyboard controller, in scan code set
   --  1. Result (Result'First .. Last) are the characters it produces,
   --  which may be none. An E0 prefix selects the extended keys for the
   --  next byte (#397).

end Keyboard.Codes;
