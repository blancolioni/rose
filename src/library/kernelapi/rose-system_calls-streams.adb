package body Rose.System_Calls.Streams is

   --------------------------
   -- Copy_From_Memory_Cap --
   --------------------------

   procedure Copy_From_Memory_Cap
     (Memory_Cap : Rose.Capabilities.Capability;
      Target     : out Ada.Streams.Stream_Element_Array)
   is
      use System.Storage_Elements, Ada.Streams;
      Buffer : System.Storage_Elements.Storage_Array
        (1 .. Storage_Count (Target'Length));
   begin
      Copy_From_Memory_Cap (Memory_Cap, Buffer);
      for I in Buffer'Range loop
         Target (Stream_Element_Offset (I - Buffer'First) + Target'First) :=
           Stream_Element (Buffer (I));
      end loop;
   end Copy_From_Memory_Cap;

   ------------------------
   -- Copy_To_Memory_Cap --
   ------------------------

   procedure Copy_To_Memory_Cap
     (Memory_Cap : Rose.Capabilities.Capability;
      Source     : Ada.Streams.Stream_Element_Array)
   is
      use System.Storage_Elements, Ada.Streams;
      Buffer : System.Storage_Elements.Storage_Array
        (1 .. Storage_Count (Source'Length));
   begin
      for I in Buffer'Range loop
         Buffer (I) :=
           Storage_Element
             (Source
                (Stream_Element_Offset (I - Buffer'First) + Source'First));
      end loop;
      Copy_To_Memory_Cap (Memory_Cap, Buffer);
   end Copy_To_Memory_Cap;

   -------------------
   -- Get_Data_Word --
   -------------------

   function Get_Data_Word
     (Parameters : in     Invocation_Record;
      Index      : in     Rose.Invocation.Invoke_Word_Index)
      return Ada.Streams.Stream_Element_Offset
   is
      Result : constant Word := Get_Data_Word (Parameters, Index);
   begin
      return Ada.Streams.Stream_Element_Offset (Result);
   end Get_Data_Word;

   -------------------
   -- Set_Data_Word --
   -------------------

   procedure Set_Data_Word
     (Parameters : in out Invocation_Record;
      Index      : in     Rose.Invocation.Invoke_Word_Index;
      Value      : in     Ada.Streams.Stream_Element_Offset)
   is
   begin
      Set_Data_Word (Parameters, Index, Rose.Words.Word (Value));
   end Set_Data_Word;

end Rose.System_Calls.Streams;
