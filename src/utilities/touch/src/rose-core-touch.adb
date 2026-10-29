with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Interfaces.Directory;
with Rose.Interfaces.Writable_File.Client;

procedure Rose.Core.Touch
  (Name      : String;
   Directory : Rose.Interfaces.Writable_Directory.Client
                 .Writable_Directory_Client)
is
   use Rose.Interfaces.Writable_Directory.Client;
   use type Rose.Capabilities.Capability;
   use type Rose.Interfaces.Directory.File_Kind;

   procedure Fail (Reason : String);
   --  Report that the file cannot be touched, and why, on standard error,
   --  and fail the launch call.

   ----------
   -- Fail --
   ----------

   procedure Fail (Reason : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "touch: cannot touch '" & Name & "'" & Reason);

      --  An unhandled exception fails the launch call (see
      --  Rose.Command_Line.Launch).

      raise Constraint_Error;
   end Fail;

   Index : constant Natural := Find_Entry (Directory, Name);

begin

   --  Create_File returns the file if Name is already one, but nothing
   --  if Name is a directory.

   if Index > 0
     and then Directory_Entry_Kind (Directory, Index)
                = Rose.Interfaces.Directory.Directory
   then
      Fail (": it is a directory");
   elsif Rose.Interfaces.Writable_File.Client.Get_Interface_Cap
           (Create_File (Directory, Name))
     = Rose.Capabilities.Null_Capability
   then
      Fail ("");
   end if;
end Rose.Core.Touch;
