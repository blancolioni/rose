with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;

package Rose.Console_IO is

   procedure Open
     (Console_Cap : Rose.Capabilities.Capability);

   procedure Put (Ch : Character);
   procedure Put (Text : String);
   procedure Put (Num : Integer);
   procedure Put (X     : Natural;
                  Width : Positive;
                  Pad   : Character := ' ');
   procedure Put (Item : Rose.Words.Word_4);
   procedure Put (Item : Rose.Words.Word_8);
   procedure Put (Item : Rose.Words.Word_16);
   procedure Put (Item : Rose.Words.Word_32);
   procedure Put (Item : Rose.Words.Word_64);
   procedure Put (Item : Rose.Objects.Object_Id);
   procedure Put (Item : System.Storage_Elements.Storage_Array);
   procedure Put_Line (Text : String);
   procedure New_Line;

   procedure Flush;

end Rose.Console_IO;
