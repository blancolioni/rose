with Ada.Text_IO;

with Rose.Interfaces.Directory;
with Rose.Interfaces.Objects;

procedure Rose.Core.Remove
  (Name      : String;
   Directory : in out
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class)
is
   use type Rose.Interfaces.Directory.File_Kind;

   Index : constant Natural := Directory.Find_Entry (Name);

   procedure Fail (Reason : String);
   --  Report that the file cannot be removed, and why, on standard error,
   --  and fail the launch call.

   ----------
   -- Fail --
   ----------

   procedure Fail (Reason : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "remove: cannot remove '" & Name & "'" & Reason);
      Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);

      --  An unhandled exception fails the launch call (see
      --  Rose.Command_Line.Launch).

      raise Constraint_Error;
   end Fail;

begin

   --  The directory refuses these too, but its error doesn't say why, so
   --  they are checked first, for the message.

   if Index = 0 then
      Fail (": there is no such entry");
   elsif Directory.Directory_Entry_Kind (Index)
     /= Rose.Interfaces.Directory.Ordinary_File
   then
      Fail (": it is not a file");
   end if;

   begin
      Directory.Delete_File (Name);
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Fail ("");
   end;
end Rose.Core.Remove;
