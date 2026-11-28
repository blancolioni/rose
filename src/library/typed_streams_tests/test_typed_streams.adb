with Ada.Command_Line;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Rose.Typed_Streams;

with Rose.Interfaces.Core.Files;
with Rose.Interfaces.Core.Units;
with Rose.Interfaces.Files.Directory;
with Rose.Interfaces.Typed_Test;

with Test_Pipes; use Test_Pipes;

procedure Test_Typed_Streams is

   --  The tests of Rose.Typed_Streams (#140), on the host: lists through
   --  a pipe in memory that gives a few bytes to each Read.

   use Ada.Strings.Unbounded;
   use type Ada.Streams.Stream_Element_Offset;

   package Files renames Rose.Interfaces.Core.Files;
   package Test renames Rose.Interfaces.Typed_Test;

   use type Files.File_Info;

   package File_Streams is
     new Rose.Typed_Streams
       (Element        => Files.File_Info,
        Type_Id        => Files.File_Info_Type_Id,
        Fields         => Files.File_Info_Fields,
        Encoded_Length => Files.Encoded_Length,
        Encode         => Files.Encode,
        Decode         => Files.Decode);

   package Entry_Streams is
     new Rose.Typed_Streams
       (Element        => Test.Entry_Info,
        Type_Id        => Test.Entry_Info_Type_Id,
        Fields         => Test.Entry_Info_Fields,
        Encoded_Length => Test.Encoded_Length,
        Encode         => Test.Encode,
        Decode         => Test.Decode);

   Failures : Natural := 0;

   procedure Check (OK : Boolean; Name : String);

   function Info (Name : String; Size : Natural) return Files.File_Info
   is ((Name => To_Unbounded_String (Name),
        Kind => Rose.Interfaces.Files.Directory.Ordinary_File,
        Size => Rose.Interfaces.Core.Units.Bytes (Size)));

   -----------
   -- Check --
   -----------

   procedure Check (OK : Boolean; Name : String) is
   begin
      if OK then
         Ada.Text_IO.Put_Line ("PASS " & Name);
      else
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

begin

   --  A hundred records through a small writer and a smaller reader.

   declare
      Channel : aliased Pipe;
      Output  : File_Streams.Writer (Channel'Access, 64);
      Input   : File_Streams.Reader (Channel'Access, 32);
      Got     : Files.File_Info;
      Same    : Boolean := True;
   begin
      for Index in 1 .. 100 loop
         Output.Put (Info ("file" & Natural'Image (Index), Index * 512));
      end loop;
      Output.Flush;

      for Index in 1 .. 100 loop
         Input.Get (Got);
         Same := Same
           and then Got = Info ("file" & Natural'Image (Index), Index * 512);
      end loop;
      Check (Same and then Input.End_Of_List, "a hundred records");
   end;

   --  A record longer than both buffers.

   declare
      Channel : aliased Pipe;
      Output  : File_Streams.Writer (Channel'Access, 64);
      Input   : File_Streams.Reader (Channel'Access, 32);
      Long    : constant Files.File_Info :=
        Info ((1 .. 300 => 'x'), 1);
      Got     : Files.File_Info;
   begin
      Output.Put (Info ("a", 1));
      Output.Put (Long);
      Output.Put (Info ("b", 2));
      Output.Flush;
      Input.Get (Got);
      Check (Got = Info ("a", 1), "before a long record");
      Input.Get (Got);
      Check (Got = Long, "a long record");
      Input.Get (Got);
      Check (Got = Info ("b", 2) and then Input.End_Of_List,
             "after a long record");
   end;

   --  An empty list, and an empty stream.

   declare
      Channel : aliased Pipe;
      Output  : File_Streams.Writer (Channel'Access, 64);
      Input   : File_Streams.Reader (Channel'Access, 32);
   begin
      Output.Flush;
      Check (Channel.Last > 0 and then Input.End_Of_List, "an empty list");
   end;

   declare
      Channel : aliased Pipe;
      Input   : File_Streams.Reader (Channel'Access, 32);
   begin
      Check (Input.End_Of_List, "an empty stream");
   end;

   --  A list of another record, and bytes that aren't a list.

   declare
      Channel : aliased Pipe;
      Output  : File_Streams.Writer (Channel'Access, 64);
      Input   : Entry_Streams.Reader (Channel'Access, 32);
   begin
      Output.Put (Info ("a", 1));
      Output.Flush;
      Check (Input.End_Of_List, "another record (expected Format_Error)");
   exception
      when Entry_Streams.Format_Error =>
         Check (True, "another record");
   end;

   declare
      Channel : aliased Pipe;
      Input   : File_Streams.Reader (Channel'Access, 32);
   begin
      Put_Bytes (Channel, (1 .. 40 => Character'Pos ('t')));
      Check (Input.End_Of_List, "not a list (expected Format_Error)");
   exception
      when File_Streams.Format_Error =>
         Check (True, "not a list");
   end;

   --  A list from a program whose Entry_Info has only Name, and from one
   --  whose Entry_Info has a field after Count.

   declare
      use type Test.Entry_Info;
      Probe   : aliased Pipe;
      Channel : aliased Pipe;
      Input   : Entry_Streams.Reader (Channel'Access, 32);
      Got     : Test.Entry_Info;
   begin
      --  The format word and type id of a list of Entry_Info, from an
      --  empty one, and an empty field table.
      declare
         Output : Entry_Streams.Writer (Probe'Access, 64);
      begin
         Output.Flush;
      end;
      Channel.Data (1 .. 12) := Probe.Data (1 .. 12);
      Channel.Last := 12;
      Put_Word (Channel, 0);

      --  Name only: header, "ab".
      Put_Word (Channel, 12 + 1 * 2 ** 24);
      Put_Word (Channel, 2);
      Put_Bytes (Channel, (Character'Pos ('a'), Character'Pos ('b'), 0, 0));

      --  Name, Count, and a word after them.
      Put_Word (Channel, 20 + 3 * 2 ** 24);
      Put_Word (Channel, 2);
      Put_Bytes (Channel, (Character'Pos ('c'), Character'Pos ('d'), 0, 0));
      Put_Word (Channel, 3);
      Put_Word (Channel, 99);

      Input.Get (Got);
      Check (Got = (Name => To_Unbounded_String ("ab"), Count => 7),
             "a record without a field takes its default");
      Input.Get (Got);
      Check (Got = (Name => To_Unbounded_String ("cd"), Count => 3)
             and then Input.End_Of_List,
             "a record with a field more skips it");
   end;

   --  A list cut short in the middle of a record.

   declare
      Channel : aliased Pipe;
      Output  : File_Streams.Writer (Channel'Access, 64);
      Input   : File_Streams.Reader (Channel'Access, 32);
      Got     : Files.File_Info;
   begin
      Output.Put (Info ("a long enough name", 1));
      Output.Flush;
      Channel.Last := Channel.Last - 3;
      Input.Get (Got);
      Check (False, "cut short (expected Format_Error)");
   exception
      when File_Streams.Format_Error =>
         Check (True, "cut short");
   end;

   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Typed_Streams;
