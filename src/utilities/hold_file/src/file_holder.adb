with Ada.Streams;
with Ada.Text_IO;
with System.Storage_Elements;

with Rose.Core.File_Client;
with Rose.Server;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;

with Rose.Interfaces.Commands.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Releasable_Reader;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Releasable_Writer;
with Rose.Interfaces.Releasable_Writer.Client;
with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.Writable_File.Client;

package body File_Holder is

   use Ada.Streams;
   use System.Storage_Elements;
   type Holder_Object is
     limited new Rose.Interfaces.Restorable.Server.Restorable_Interface
   with null record;

   overriding function Restorable_Cap
     (Item : in out Holder_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Restorable.Server.Cap_Of (Item));

   overriding procedure Restored (Item : in out Holder_Object);

   Context    : Rose.Server.Server_Context;
   The_Holder : aliased Holder_Object;

   File   : Rose.Interfaces.Writable_File.Client.Writable_File_Client;
   Writer : Rose.Interfaces.Releasable_Writer.Client.Releasable_Writer_Client;
   Reader : Rose.Interfaces.Releasable_Reader.Client.Releasable_Reader_Client;
   --  The file, and the streams that are open across the handover and
   --  the restore.

   Checked : Boolean := False;
   --  Set once the file has been checked after a restore. The check
   --  writes to it, so it is made once; the variable is saved with the
   --  rest of the process.

   function Bytes (Text : String) return Stream_Element_Array;

   function Read_All return String;
   --  What Reader reads until it reaches the end.

   -----------
   -- Bytes --
   -----------

   function Bytes (Text : String) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. Text'Length);
   begin
      for I in Text'Range loop
         Result (Stream_Element_Offset (I - Text'First + 1)) :=
           Character'Pos (Text (I));
      end loop;
      return Result;
   end Bytes;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Commands        : Rose.Capabilities.Capability;
      Cap_Set_Service : Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability)
   is
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Programs : Rose.Interfaces.Commands.Client.Commands_Client;
      Endpoint : Rose.Capabilities.Capability;
      Served   : Rose.Capabilities.Capability;
   begin
      Control := Rose.Capabilities.Null_Capability;
      Rose.Interfaces.Restorable.Server.Attach (Context, The_Holder'Access);
      Programs.Open (Commands);

      declare
         Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
           Programs.Find ("file");
      begin
         Endpoint :=
           Launcher.Launch_Command (Rose.System_Calls.Own_Bank.Cap, No_Caps);
         Rose.Core.File_Client.Call
           (Endpoint        => Endpoint,
            Create_Endpoint => Rose.System_Calls.Create_Endpoint_Capability,
            Delete_Cap      => Rose.System_Calls.Delete_Cap_Capability,
            Cap_Set_Service => Cap_Set_Service,
            Served          => Served,
            Control         => Control);
         File.Open (Served);

         declare
            First : Rose.Interfaces.Releasable_Writer
              .Releasable_Writer_Object'Class := File.Rewrite;
         begin
            First.Write (Bytes ("hello"));
            First.Release;
         end;

         declare
            Appender : Rose.Interfaces.Releasable_Writer
              .Releasable_Writer_Object'Class := File.Append;
            Opened   : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := File.Open_Reader;
            Start    : Stream_Element_Array (1 .. 2);
            Last     : Stream_Element_Offset;
         begin
            Writer.Open (Appender.Releasable_Writer_Cap);
            Reader.Open (Opened.Releasable_Reader_Cap);
            Reader.Read (Start, Last);
            if Last /= Start'Last then
               raise Rose.Interfaces.Objects.Invocation_Error
                 with "the reader read nothing";
            end if;
         end;

         Ada.Text_IO.Put_Line
           ("hold_file: holding a file with a writer and a reader open");
      end;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Ada.Text_IO.Put_Line ("hold_file: FAIL: the file did not start");
         Control := Rose.Capabilities.Null_Capability;
   end Create_Server;

   --------------
   -- Read_All --
   --------------

   function Read_All return String is
      Buffer : Stream_Element_Array (1 .. 64);
      Last   : Stream_Element_Offset;
      Result : String (1 .. 256);
      Length : Natural := 0;
   begin
      loop
         Reader.Read (Buffer, Last);
         exit when Last < Buffer'First;
         for Item of Buffer (Buffer'First .. Last) loop
            exit when Length = Result'Last;
            Length := Length + 1;
            Result (Length) := Character'Val (Item);
         end loop;
      end loop;
      return Result (1 .. Length);
   end Read_All;

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Holder_Object) is
      pragma Unreferenced (Item);
   begin
      if Checked then
         return;
      end if;
      Checked := True;

      declare
         Size_Before : constant Storage_Count := File.Size;
      begin
         Writer.Write (Bytes (" restored"));
         declare
            Rest       : constant String := Read_All;
            Size_After : constant Storage_Count := File.Size;
         begin
            if Size_Before = 5
              and then Rest = "llo restored"
              and then Size_After = 14
            then
               Ada.Text_IO.Put_Line
                 ("hold_file: restored: the replaced file has its "
                  & "contents, and its writer and reader work");
            else
               Ada.Text_IO.Put_Line
                 ("hold_file: FAIL: restored: size"
                  & Storage_Count'Image (Size_Before)
                  & ", then read """ & Rest & """, then size"
                  & Storage_Count'Image (Size_After));
            end if;
         end;
      end;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Ada.Text_IO.Put_Line
           ("hold_file: FAIL: restored: a call to the file failed");
   end Restored;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end File_Holder;
