with Ada.IO_Exceptions;
with Ada.Text_IO.Text_Streams;

with GNAT.OS_Lib;

with Rose.Command_Line;
with Rose.Interfaces.Objects;

package body Rose.Host.Streams is

   use Ada.Streams;
   use type Rose.Words.Word_64;

   ----------
   -- Open --
   ----------

   procedure Open
     (Item : in out Reader;
      Name : String)
   is
   begin
      if Name = "" then
         Item.Standard_Input := True;
      else
         Open_Path (Item, Name);
      end if;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         raise Rose.Command_Line.Command_Failed
           with "no file named '" & Name & "'";
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (Item : in out Writer;
      Name : String)
   is
   begin
      if Name = "" then
         Item.Standard_Output := True;
      else
         Open_Path (Item, Name, Append => False);
      end if;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         raise Rose.Command_Line.Command_Failed
           with "cannot write '" & Name & "'";
   end Open;

   ---------------
   -- Open_Path --
   ---------------

   procedure Open_Path
     (Item : in out Reader;
      Path : String)
   is
   begin
      Stream_IO.Open (Item.File, Stream_IO.In_File, Path);
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         raise Rose.Interfaces.Objects.Invocation_Error
           with "cannot read " & Path;
   end Open_Path;

   ---------------
   -- Open_Path --
   ---------------

   procedure Open_Path
     (Item   : in out Writer;
      Path   : String;
      Append : Boolean)
   is
   begin
      if Append then
         Stream_IO.Open (Item.File, Stream_IO.Append_File, Path);
      else
         Stream_IO.Create (Item.File, Stream_IO.Out_File, Path);
      end if;
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         raise Rose.Interfaces.Objects.Invocation_Error
           with "cannot write " & Path;
   end Open_Path;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Reader;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      if Item.Standard_Input then
         declare
            Count : constant Integer :=
              GNAT.OS_Lib.Read
                (GNAT.OS_Lib.Standin, Buffer'Address, Buffer'Length);
         begin
            if Count < 0 then
               raise Rose.Interfaces.Objects.Invocation_Error
                 with "cannot read standard input";
            end if;
            Last := Buffer'First + Stream_Element_Offset (Count) - 1;
         end;
      elsif Stream_IO.Is_Open (Item.File) then
         Stream_IO.Read (Item.File, Buffer, Last);
      else
         raise Rose.Interfaces.Objects.Invocation_Error
           with "the reader is released";
      end if;
   end Read;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out Reader) is
   begin
      if Stream_IO.Is_Open (Item.File) then
         Stream_IO.Close (Item.File);
      end if;
      Item.Standard_Input := False;
   end Release;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out Writer) is
   begin
      if Stream_IO.Is_Open (Item.File) then
         Stream_IO.Close (Item.File);
      end if;
      Item.Standard_Output := False;
   end Release;

   ---------------------------
   -- Releasable_Reader_Cap --
   ---------------------------

   overriding function Releasable_Reader_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   ---------------------------
   -- Releasable_Writer_Cap --
   ---------------------------

   overriding function Releasable_Writer_Cap
     (Item : in out Writer)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   ----------------
   -- Reader_Cap --
   ----------------

   overriding function Reader_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   ------------------
   -- Resource_Cap --
   ------------------

   overriding function Resource_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   ------------------
   -- Resource_Cap --
   ------------------

   overriding function Resource_Cap
     (Item : in out Writer)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   ----------
   -- Seek --
   ----------

   overriding procedure Seek
     (Item              : in out Reader;
      Offset_From_Start : Rose.Words.Word_64)
   is
   begin
      if Item.Standard_Input or else not Stream_IO.Is_Open (Item.File) then
         raise Rose.Interfaces.Objects.Invocation_Error
           with "the reader can't seek";
      end if;
      Stream_IO.Set_Index
        (Item.File, Stream_IO.Positive_Count (Offset_From_Start + 1));
   end Seek;

   ------------------
   -- Seekable_Cap --
   ------------------

   overriding function Seekable_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   -------------------------
   -- Seekable_Reader_Cap --
   -------------------------

   overriding function Seekable_Reader_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : in out Writer;
      Buffer : Ada.Streams.Stream_Element_Array)
   is
   begin
      if Item.Standard_Output then

         --  Through Text_IO's stream, so that what the command writes
         --  with Text_IO and with this stay in order.

         Ada.Streams.Write
           (Ada.Text_IO.Text_Streams.Stream
              (Ada.Text_IO.Standard_Output).all,
            Buffer);
      elsif Stream_IO.Is_Open (Item.File) then
         Stream_IO.Write (Item.File, Buffer);
      else
         raise Rose.Interfaces.Objects.Invocation_Error
           with "the writer is released";
      end if;
   end Write;

   ----------------
   -- Writer_Cap --
   ----------------

   overriding function Writer_Cap
     (Item : in out Writer)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

end Rose.Host.Streams;
