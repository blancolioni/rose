with Ada.Text_IO;

with Rose.Capabilities;

procedure Rose.Core.Make_Directory
  (Directory_Name : String;
   Directory      : Rose.Interfaces.Writable_Directory.Client
                      .Writable_Directory_Client)
is
   use Rose.Interfaces.Writable_Directory.Client;
   use type Rose.Capabilities.Capability;

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

      --  An unhandled exception fails the launch call (see
      --  Rose.Command_Line.Launch).

      raise Constraint_Error;
   end Fail;

begin

   --  Create_Directory returns the directory if the name is already one,
   --  but a directory that exists is an error here.

   if Find_Entry (Directory, Directory_Name) > 0 then
      Fail (": it already exists");
   elsif Get_Interface_Cap (Create_Directory (Directory, Directory_Name))
     = Rose.Capabilities.Null_Capability
   then
      Fail ("");
   end if;
end Rose.Core.Make_Directory;
