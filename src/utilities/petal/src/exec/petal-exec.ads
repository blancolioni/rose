with Petal.Commands;

package Petal.Exec is

   function Find
     (Name : String)
      return Petal.Commands.Petal_Command;

   function Is_Built_In (Name : String) return Boolean;
   --  True if Name is the name of a built-in command.

   procedure Install
     (Name    : String;
      Command : Petal.Commands.Petal_Command);

end Petal.Exec;
