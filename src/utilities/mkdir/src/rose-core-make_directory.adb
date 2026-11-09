with Rose.Command_Line;

with Rose.Interfaces.Objects;

procedure Rose.Core.Make_Directory
  (Directory_Name : String;
   Directory      : in out
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class)
is
   procedure Fail (Reason : String)
     with No_Return;
   --  Fail the launch call with a message that says that the directory cannot
   --  be created, and why (#802).

   ----------
   -- Fail --
   ----------

   procedure Fail (Reason : String) is
   begin
      raise Rose.Command_Line.Command_Failed
        with "cannot create directory '" & Directory_Name & "'"
        & Reason;
   end Fail;

begin

   --  Create_Directory returns the directory if the name is already one,
   --  but a directory that exists is an error here.

   if Directory.Find_Entry (Directory_Name) > 0 then
      Fail (": it already exists");
   end if;

   begin
      declare
         Created : Rose.Interfaces.Writable_Directory
           .Writable_Directory_Object'Class :=
             Directory.Create_Directory (Directory_Name);
         pragma Unreferenced (Created);
      begin
         null;
      end;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Fail ("");
   end;
end Rose.Core.Make_Directory;
