with Petal.Commands;

package Petal.Exec is

   function Find
     (Name : String)
      return Petal.Commands.Petal_Command;

   procedure Install
     (Name    : String;
      Command : Petal.Commands.Petal_Command);

end Petal.Exec;
