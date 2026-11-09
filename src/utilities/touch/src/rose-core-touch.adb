with Rose.Command_Line;

with Rose.Interfaces.Directory;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Writable_File;

procedure Rose.Core.Touch
  (Name      : String;
   Directory : in out
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class)
is
   use type Rose.Interfaces.Directory.File_Kind;

   Index : constant Natural := Directory.Find_Entry (Name);

   procedure Fail (Reason : String)
     with No_Return;
   --  Fail the launch call with a message that says that the file cannot be
   --  touched, and why (#802).

   ----------
   -- Fail --
   ----------

   procedure Fail (Reason : String) is
   begin
      raise Rose.Command_Line.Command_Failed
        with "cannot touch '" & Name & "'" & Reason;
   end Fail;

begin

   --  Create_File returns the file if Name is already one, but fails if
   --  Name is a directory, so that is checked first, for the message.

   if Index > 0
     and then Directory.Directory_Entry_Kind (Index)
                = Rose.Interfaces.Directory.Directory
   then
      Fail (": it is a directory");
   end if;

   begin
      declare
         File : Rose.Interfaces.Writable_File.Writable_File_Object'Class :=
           Directory.Create_File (Name);
         pragma Unreferenced (File);
      begin
         null;
      end;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Fail ("");
   end;
end Rose.Core.Touch;
