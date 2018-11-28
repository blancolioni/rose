with System.Storage_Elements;
with Rose.Words;

package Rose.Devices is

   procedure To_Storage
     (Target : in out System.Storage_Elements.Storage_Array;
      Last   : in out System.Storage_Elements.Storage_Offset;
      Value  : Rose.Words.Word_64);

end Rose.Devices;
