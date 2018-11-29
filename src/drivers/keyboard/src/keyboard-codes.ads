with System.Storage_Elements;
with Rose.Words;

package Keyboard.Codes is

   procedure Handle_Key
     (Code    : Rose.Words.Word;
      Pressed : Boolean;
      Result  : out System.Storage_Elements.Storage_Array;
      Last    : out System.Storage_Elements.Storage_Count);

end Keyboard.Codes;
