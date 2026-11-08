with Ada.Text_IO;

with Rose.Interfaces.Objects;

procedure Rose.Core.Make_Directory
  (Directory_Name : String;
   Directory      : in out
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class)
is
   procedure Fail (Reason : String);
   --  Report that the directory cannot be created, and why, on standard
   --  error, and fail the launch call.

   ----------
   -- Fail --
   ----------

   procedure Fail (Reason : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "mkdir: cannot create directory '" & Directory_Name & "'"
         & Reason);
      Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);

      --  An unhandled exception fails the launch call (see
      --  Rose.Command_Line.Launch).

      raise Constraint_Error;
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
