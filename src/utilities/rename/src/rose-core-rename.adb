with Rose.Command_Line;

with Rose.Interfaces.Objects;

procedure Rose.Core.Rename
  (From      : String;
   To        : String;
   Directory : in out
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class)
is

   procedure Fail (Reason : String)
     with No_Return;
   --  Fail the launch call with a message that says that the entry cannot be
   --  renamed, and why (#802).

   ----------
   -- Fail --
   ----------

   procedure Fail (Reason : String) is
   begin
      raise Rose.Command_Line.Command_Failed
        with "cannot rename '" & From & "' to '" & To & "'" & Reason;
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
