with Ada.Streams;

package Rose.System_Calls.Streams is

   procedure Set_Data_Word
     (Parameters : in out Invocation_Record;
      Index      : in     Rose.Invocation.Invoke_Word_Index;
      Value      : in     Ada.Streams.Stream_Element_Offset);

   function Get_Data_Word
     (Parameters : in     Invocation_Record;
      Index      : in     Rose.Invocation.Invoke_Word_Index)
      return Ada.Streams.Stream_Element_Offset;

   procedure Copy_From_Memory_Cap
     (Memory_Cap : Rose.Capabilities.Capability;
      Target     : out Ada.Streams.Stream_Element_Array);

   procedure Copy_To_Memory_Cap
     (Memory_Cap : Rose.Capabilities.Capability;
      Source     : Ada.Streams.Stream_Element_Array);

end Rose.System_Calls.Streams;
