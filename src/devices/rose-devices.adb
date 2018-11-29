package body Rose.Devices is

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Target : in out System.Storage_Elements.Storage_Array;
      Last   : in out System.Storage_Elements.Storage_Offset;
      Value  : Rose.Words.Word_64)
   is
      use Rose.Words;
      use type System.Storage_Elements.Storage_Offset;
      It : Word_64 := Value;
   begin
      for I in 1 .. 8 loop
         Last := Last + 1;
         Target (Last) := System.Storage_Elements.Storage_Element (It mod 256);
         It := It / 256;
      end loop;
   end To_Storage;

end Rose.Devices;
