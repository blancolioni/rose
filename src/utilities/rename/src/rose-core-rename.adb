with Ada.Text_IO;

with Rose.Interfaces.Objects;

procedure Rose.Core.Rename
  (From      : String;
   To        : String;
   Directory : in out
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class)
is

   procedure Fail (Reason : String);
   --  Report that the entry cannot be renamed, and why, on standard
   --  error, and fail the launch call.

   ----------
   -- Fail --
   ----------

   procedure Fail (Reason : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "rename: cannot rename '" & From & "' to '" & To & "'" & Reason);
      Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);

      --  An unhandled exception fails the launch call (see
      --  Rose.Command_Line.Launch).

      raise Constraint_Error;
   end Fail;

begin

   --  The directory refuses these too, but its error doesn't say why, so
   --  they are checked first, for the message.

   if Directory.Find_Entry (From) = 0 then
      Fail (": there is no such entry");
   elsif Directory.Find_Entry (To) > 0 then
      Fail (": it already exists");
   end if;

   begin
      Directory.Rename (From, To);
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Fail ("");
   end;
end Rose.Core.Rename;
