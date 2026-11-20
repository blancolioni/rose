with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with System.Storage_Elements;

with Rose.Interfaces.Common.Named.Server;
with Rose.Interfaces.Streams.Writer.Client;
with Rose.Interfaces.Streams.Writer.Server;
with Rose.Invocation;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

package body Page_Probe_Server is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;
   use type Rose.Words.Word_32;

   Page_Size   : constant := 4096;
   Chunk_Pages : constant := 1024;
   --  The filler takes a checkpoint after each chunk.

   Max_Chunks  : constant := 10;
   --  40 MiB, which is more than the 32 MiB machine of the boot tests
   --  has (scripts/qemu-test-lib.sh).

   Page_Words  : constant := Page_Size / 4;

   Buffer_Pages : constant := 4;
   --  The caller's buffer. It is page aligned, so the kernel maps it in
   --  four of the filler's eight shared slots (#519).

   Probe_Failed : exception;

   Context : Rose.Server.Server_Context;

   type Named_Object is
     limited new Rose.Interfaces.Common.Named.Server.Named_Interface
   with null record;

   overriding function Named_Cap
     (Item : in out Named_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Common.Named.Server.Cap_Of (Context, Item));

   overriding function Name (Item : in out Named_Object) return String;

   type Writer_Object is
     limited new Rose.Interfaces.Streams.Writer.Server.Writer_Interface
   with null record;

   overriding function Writer_Cap
     (Item : in out Writer_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Streams.Writer.Server.Cap_Of (Context, Item));

   overriding procedure Write
     (Item   : in out Writer_Object;
      Buffer : Ada.Streams.Stream_Element_Array);

   The_Named  : aliased Named_Object;
   The_Writer : aliased Writer_Object;

   Fill_Pages     : Natural := 0;
   Checkpoint_Cap : Rose.Capabilities.Capability :=
                      Rose.Capabilities.Null_Capability;
   Target_Cap     : Rose.Capabilities.Capability :=
                      Rose.Capabilities.Null_Capability;

   Fill_Area : array (0 .. Max_Chunks * Chunk_Pages * Page_Words - 1)
     of Rose.Words.Word_32
     with Alignment => Page_Size;
   --  The memory that the filler fills. It is in the program's bss, since
   --  a program's heap is at most 1 MiB (Elf.Loader.Heap_Size). Nothing
   --  touches it until Write, which faults its pages in a chunk at a
   --  time.

   Buffer : Ada.Streams.Stream_Element_Array
     (1 .. Buffer_Pages * Page_Size)
     with Alignment => Page_Size;

   Result      : String (1 .. 8);
   Result_Last : Natural := 0;

   function Buffer_Element
     (Index : Ada.Streams.Stream_Element_Offset)
      return Ada.Streams.Stream_Element
   is (Ada.Streams.Stream_Element ((Index * 7 + 3) mod 251));

   function Buffer_Is_Right
     (Data : Ada.Streams.Stream_Element_Array)
      return Boolean;

   function Pattern
     (Page : Natural;
      Word : Natural)
      return Rose.Words.Word_32
   is (Rose.Words.Word_32 (Page) * 16#9E37_79B1# + Rose.Words.Word_32 (Word)
       + 16#5A5A_0000#);
   --  What word Word of the filler's page Page holds.

   procedure Fill_Chunk (Chunk : Positive);
   function Bad_Pages (Chunk : Positive) return Natural;

   procedure Fail (Message : String)
     with No_Return;
   --  Say Message on standard output, and raise Probe_Failed.

   function Image (X : Natural) return String;
   --  X, with no leading space.

   function Hex (X : Rose.Words.Word_32) return String;
   function Hex (X : System.Address) return String;
   --  X in hexadecimal, for the report of a page that is wrong.

   procedure Take_Checkpoint;
   --  Through init's Take_Checkpoint: a checkpoint saves the pages that
   --  were filled, so they are clean.

   ---------------
   -- Bad_Pages --
   ---------------

   function Bad_Pages (Chunk : Positive) return Natural is
      Bad : Natural := 0;
   begin
      for Page in 0 .. Chunk_Pages - 1 loop
         declare
            Global : constant Natural := (Chunk - 1) * Chunk_Pages + Page;
            First  : constant Natural := Global * Page_Words;
         begin
            if Fill_Area (First) /= Pattern (Global, 0)
              or else Fill_Area (First + 511) /= Pattern (Global, 511)
              or else Fill_Area (First + 1023) /= Pattern (Global, 1023)
            then
               if Bad < 4 then
                  Ada.Text_IO.Put_Line
                    ("page_probe: page " & Image (Global)
                     & " at " & Hex (Fill_Area (First)'Address)
                     & " holds " & Hex (Fill_Area (First))
                     & " " & Hex (Fill_Area (First + 511))
                     & " " & Hex (Fill_Area (First + 1023))
                     & "; expected " & Hex (Pattern (Global, 0)));
               end if;
               Bad := Bad + 1;
            end if;
         end;
      end loop;
      return Bad;
   end Bad_Pages;

   ---------------------
   -- Buffer_Is_Right --
   ---------------------

   function Buffer_Is_Right
     (Data : Ada.Streams.Stream_Element_Array)
      return Boolean
   is
   begin
      if Data'Length /= Buffer'Length then
         return False;
      end if;

      for I in Data'Range loop
         if Data (I) /= Buffer_Element (I - Data'First + 1) then
            return False;
         end if;
      end loop;
      return True;
   end Buffer_Is_Right;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Pages      : Natural;
      Checkpoint : Rose.Capabilities.Capability;
      Target     : Rose.Capabilities.Capability)
   is
   begin
      Fill_Pages := Natural'Min (Pages, Max_Chunks * Chunk_Pages);
      Checkpoint_Cap := Checkpoint;
      Target_Cap := Target;

      for I in Buffer'Range loop
         Buffer (I) := Buffer_Element (I);
      end loop;

      Rose.Interfaces.Common.Named.Server.Attach (Context, The_Named'Access);
      Rose.Interfaces.Streams.Writer.Server.Attach
        (Context, The_Writer'Access);
   end Create_Server;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String) is
   begin
      Ada.Text_IO.Put_Line ("page_probe: " & Message);
      raise Probe_Failed;
   end Fail;

   ----------------
   -- Fill_Chunk --
   ----------------

   procedure Fill_Chunk (Chunk : Positive) is
   begin
      for Page in 0 .. Chunk_Pages - 1 loop
         declare
            Global : constant Natural := (Chunk - 1) * Chunk_Pages + Page;
            First  : constant Natural := Global * Page_Words;
         begin
            Fill_Area (First) := Pattern (Global, 0);
            Fill_Area (First + 511) := Pattern (Global, 511);
            Fill_Area (First + 1023) := Pattern (Global, 1023);
         end;
      end loop;
   end Fill_Chunk;

   ---------
   -- Hex --
   ---------

   function Hex (X : Rose.Words.Word_32) return String is
      Digit : constant String := "0123456789ABCDEF";
      Text  : String (1 .. 8);
      Y     : Rose.Words.Word_32 := X;
   begin
      for I in reverse Text'Range loop
         Text (I) := Digit (Natural (Y mod 16) + 1);
         Y := Y / 16;
      end loop;
      return Text;
   end Hex;

   function Hex (X : System.Address) return String is
   begin
      return Hex (Rose.Words.Word_32
                  (System.Storage_Elements.To_Integer (X)));
   end Hex;

   -----------
   -- Image --
   -----------

   function Image (X : Natural) return String is
      Text : constant String := Natural'Image (X);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   ----------
   -- Name --
   ----------

   overriding function Name (Item : in out Named_Object) return String is
      pragma Unreferenced (Item);

      procedure Set_Result (Text : String);

      ----------------
      -- Set_Result --
      ----------------

      procedure Set_Result (Text : String) is
      begin
         Result (1 .. Text'Length) := Text;
         Result_Last := Text'Length;
      end Set_Result;

   begin
      if Result_Last = 0 then
         declare
            Filler : Rose.Interfaces.Streams.Writer.Client.Writer_Client;
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.Interfaces.Streams.Writer.Client.Open (Filler, Target_Cap);

            --  Not the client's Write, which copies the data into this
            --  process's invoke buffer: the kernel is to lend the filler
            --  Buffer's own pages, which the checkpoint has made clean.

            Rose.System_Calls.Initialize_Send
              (Params,
               Rose.Interfaces.Streams.Writer.Client.Get_Write_Cap (Filler));
            Rose.System_Calls.Send_Buffer
              (Params, Buffer'Length, Buffer'Address, False);
            Rose.System_Calls.Invoke_Capability (Params);

            if Params.Control.Flags (Rose.Invocation.Error) then
               Ada.Text_IO.Put_Line
                 ("page_probe: the filler's Write failed: error word "
                  & Image (Natural (Params.Data (0))));
               Set_Result ("failed");
            else
               Set_Result ("passed");
            end if;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 ("page_probe: the call to the filler failed: "
                  & Ada.Exceptions.Exception_Name (E));
               Set_Result ("failed");
         end;
      end if;

      return Result (1 .. Result_Last);
   end Name;

   ---------------
   -- Named_Cap --
   ---------------

   function Named_Cap return Rose.Capabilities.Capability is
   begin
      return Rose.Interfaces.Common.Named.Server
        .Interface_Cap (Context, The_Named'Access);
   end Named_Cap;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   ---------------------
   -- Take_Checkpoint --
   ---------------------

   procedure Take_Checkpoint is
   begin
      for Attempt in 1 .. 5 loop
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Checkpoint_Cap);
            Rose.System_Calls.Receive_Words (Params, 1);
            Rose.System_Calls.Invoke_Capability (Params);
            if not Params.Control.Flags (Rose.Invocation.Error)
              and then Params.Control.Flags (Rose.Invocation.Send_Words)
              and then Params.Data (0) = 1
            then
               return;
            end if;
         end;
      end loop;
      Fail ("no checkpoint");
   end Take_Checkpoint;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : in out Writer_Object;
      Buffer : Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Item);
      use type Rose.Capabilities.Capability;
      Chunk_Count : constant Natural := Fill_Pages / Chunk_Pages;
      Bad         : Natural := 0;
      Filled      : Natural := 0;
   begin
      Ada.Text_IO.Put_Line
        ("page_probe: the filler's Write has a buffer of "
         & Image (Natural (Buffer'Length)) & " bytes");

      --  Buffer is the caller's, mapped in this process's shared slots
      --  for the call, so its pages are pinned (#972): mem must not drop
      --  them while the chunks below push its free pages under the
      --  reserve.

      if Chunk_Count = 0 then
         Fail ("no pages to fill");
      elsif not Buffer_Is_Right (Buffer) then
         Fail ("the buffer was wrong when the call began; length "
               & Image (Natural (Buffer'Length)));
      end if;

      --  With no checkpoint cap, nothing is checkpointed here, so mem
      --  has to page the changed pages out to the log (#974).

      for Chunk in 1 .. Chunk_Count loop
         Fill_Chunk (Chunk);
         if Checkpoint_Cap /= Rose.Capabilities.Null_Capability then
            Take_Checkpoint;
         end if;
         Filled := Chunk;
      end loop;

      for Chunk in 1 .. Chunk_Count loop
         Bad := Bad + Bad_Pages (Chunk);
      end loop;

      if Bad > 0 then
         Fail (Image (Bad) & " of" & Natural'Image (Fill_Pages)
               & " pages were wrong");
      elsif not Buffer_Is_Right (Buffer) then
         Fail ("the buffer was wrong when the call ended");
      end if;

      Ada.Text_IO.Put_Line
        ("page_probe: filled " & Image (Chunk_Count * Chunk_Pages)
         & " pages, and read them and the buffer back");

   exception
      when Probe_Failed =>
         raise;
      when E : others =>
         Ada.Text_IO.Put_Line
           ("page_probe: the filler raised "
            & Ada.Exceptions.Exception_Name (E)
            & " after" & Natural'Image (Filled) & " chunks");
         raise;
   end Write;

   ----------------
   -- Writer_Cap --
   ----------------

   function Writer_Cap return Rose.Capabilities.Capability is
   begin
      return Rose.Interfaces.Streams.Writer.Server
        .Interface_Cap (Context, The_Writer'Access);
   end Writer_Cap;

end Page_Probe_Server;
