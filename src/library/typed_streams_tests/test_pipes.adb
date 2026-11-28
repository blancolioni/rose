package body Test_Pipes is

   ---------------
   -- Put_Bytes --
   ---------------

   procedure Put_Bytes
     (Item  : in out Pipe;
      Bytes : System.Storage_Elements.Storage_Array)
   is
   begin
      for Byte of Bytes loop
         Item.Last := Item.Last + 1;
         Item.Data (Item.Last) := Stream_Element (Byte);
      end loop;
   end Put_Bytes;

   --------------
   -- Put_Word --
   --------------

   procedure Put_Word
     (Item  : in out Pipe;
      Value : Natural)
   is
      Rest : Natural := Value;
   begin
      for Index in 1 .. 4 loop
         Item.Last := Item.Last + 1;
         Item.Data (Item.Last) := Stream_Element (Rest mod 256);
         Rest := Rest / 256;
      end loop;
   end Put_Word;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Pipe;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Count : constant Stream_Element_Offset :=
        Stream_Element_Offset'Min
          (Item.Chunk,
           Stream_Element_Offset'Min
             (Buffer'Length, Item.Last - Item.Next + 1));
   begin
      Last := Buffer'First + Count - 1;
      Buffer (Buffer'First .. Last) :=
        Item.Data (Item.Next .. Item.Next + Count - 1);
      Item.Next := Item.Next + Count;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : in out Pipe;
      Buffer : Stream_Element_Array)
   is
   begin
      Item.Data (Item.Last + 1 .. Item.Last + Buffer'Length) := Buffer;
      Item.Last := Item.Last + Buffer'Length;
   end Write;

end Test_Pipes;
