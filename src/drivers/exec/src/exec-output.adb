with Ada.Streams;

with Rose.Interfaces.Objects;
with Rose.Interfaces.Writer.Client;

package body Exec.Output is

   Output : Rose.Interfaces.Writer.Client.Writer_Client;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
      Line : constant String := Text & ASCII.LF;
      Data : constant Ada.Streams.Stream_Element_Array (1 .. Line'Length)
        with Import, Address => Line'Address;
   begin
      Output.Write (Data);
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         null;
   end Put_Line;

   ---------
   -- Set --
   ---------

   procedure Set (Writer : Rose.Capabilities.Capability) is
   begin
      Rose.Interfaces.Writer.Client.Open (Output, Writer);
   end Set;

end Exec.Output;
