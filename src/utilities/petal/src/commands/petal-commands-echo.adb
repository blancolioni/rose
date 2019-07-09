with Ada.Text_IO;

with Petal.Version;

package body Petal.Commands.Echo is

   function On_Echo
     (Context   : Launch_Context;
      Arguments : Petal.Actual_Arguments.Actual_Argument_Container)
      return Petal.Values.Petal_Value;

   ------------------
   -- Echo_Command --
   ------------------

   function Echo_Command return Petal_Command is
      use Petal.Formal_Arguments;
      Named : constant Formal_Argument_Array :=
                (1 => Named_Argument
                   ("", 'n', Petal.Values.Boolean_Value, Value_Argument),
                 2 => Named_Argument
                   ("version", ' ',
                    Petal.Values.Boolean_Value, Value_Argument));
      Indexed : Formal_Argument_Array (1 .. 0);
      Extra   : constant Petal.Formal_Arguments.Formal_Argument :=
                  Positional_String_Argument
                    ("item", False, True, "");
   begin
      return Petal.Commands.Create_Built_In
        (Handler           => On_Echo'Access,
         Formal_Arguments  => Named,
         Indexed_Arguments => Indexed,
         Extra_Arguments   => Extra);
   end Echo_Command;

   -------------
   -- On_Echo --
   -------------

   function On_Echo
     (Context   : Launch_Context;
      Arguments : Petal.Actual_Arguments.Actual_Argument_Container)
      return Petal.Values.Petal_Value
   is
      Version  : constant Boolean :=
                   Petal.Actual_Arguments.To_Boolean
                     (Petal.Actual_Arguments.Named_Argument
                        (Arguments, "version"));
      New_Line : constant Boolean :=
                   not Petal.Values.To_Boolean
                     (Petal.Actual_Arguments.Named_Argument
                        (Arguments, 'n'));
      Echo_Count : constant Natural :=
                     Petal.Actual_Arguments.Extra_Argument_Count
                       (Arguments);
   begin

      if Version then
         Ada.Text_IO.Put_Line
           ("Rose petal, version "
            & Petal.Version.Version_Name);
         return Petal.Values.To_Value (True);
      end if;

      for I in 1 .. Echo_Count loop
         if I > 1 then
            Ada.Text_IO.Put (' ');
         end if;
         Ada.Text_IO.Put
           (Petal.Actual_Arguments.To_String
              (Petal.Actual_Arguments.Extra_Argument (Arguments, I)));
      end loop;
      if New_Line then
         Ada.Text_IO.New_Line;
      end if;
      return Petal.Values.To_Value (True);
   end On_Echo;

end Petal.Commands.Echo;
