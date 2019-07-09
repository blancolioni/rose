with Ada.Text_IO;

with Rose.System_Calls.Client;

with Petal.Exec;

package body Petal.Commands.Install is

   function On_Install
     (Arguments : Petal.Actual_Arguments.Actual_Argument_Array)
      return Petal.Values.Petal_Value;

   ---------------------
   -- Install_Command --
   ---------------------

   function Install_Command return Petal_Command is
   begin
      return Create_Built_In
        (Handler          => On_Install'Access,
         Formal_Arguments =>
           (1 =>
                Petal.Formal_Arguments.Positional_String_Argument
              (Key           => "command-name",
               Required      => True,
               Multiple      => False),
            2 =>
              Petal.Formal_Arguments.Positional_Read_Stream_Argument
                ("formal-arguments",
                 Required => True,
                 Multiple => False),
            3 =>
              Petal.Formal_Arguments.Positional_Read_Stream_Argument
                ("executable",
                 Required => True,
                 Multiple => False)));
   end Install_Command;

   ----------------
   -- On_Install --
   ----------------

   function On_Install
     (Arguments : Petal.Actual_Arguments.Actual_Argument_Array)
      return Petal.Values.Petal_Value
   is
      Max_Formals : constant := 20;
      Formal_Count : Natural := 0;
      Formals      : Petal.Formal_Arguments.Formal_Argument_Array
        (1 .. Max_Formals);
      Options_File : Ada.Text_IO.File_Type;

      Command_Name : constant Petal.Actual_Arguments.Actual_Argument :=
                       Arguments (Arguments'First);
      Options      : constant Petal.Actual_Arguments.Actual_Argument :=
                       Arguments (Arguments'First + 1);
      Image        : constant Petal.Actual_Arguments.Actual_Argument :=
                       Arguments (Arguments'First + 2);

      procedure Parse_Line
        (Line : String;
         Interpret : not null access
           procedure (Short_Name : String;
                      Long_Name  : String;
                      Property   : String;
                      Type_Name  : String;
                      Key        : String));

      procedure Parse_Line
        (Line      : String;
         Interpret : not null access
           procedure (Short_Name : String;
                      Long_Name  : String;
                      Property   : String;
                      Type_Name  : String;
                      Key        : String))
      is
         Start  : array (1 .. 5) of Positive := (others => Line'First);
         Finish : array (1 .. 5) of Natural := (others => Line'First - 1);
         Param   : Positive := 1;
         Current : Natural := Line'First - 1;
      begin
         for Ch of Line loop
            Current := Current + 1;
            if Ch = ',' then
               Finish (Param) := Current - 1;
               Param := Param + 1;
               exit when Param > Start'Last;
               Start (Param) := Current + 1;
            end if;
         end loop;

         if Param <= Finish'Last then
            Finish (Param) := Current;
         end if;

         Interpret
           (Line (Start (1) .. Finish (1)),
            Line (Start (2) .. Finish (2)),
            Line (Start (3) .. Finish (3)),
            Line (Start (4) .. Finish (4)),
            Line (Start (5) .. Finish (5)));
      end Parse_Line;

   begin
      Ada.Text_IO.Open (Options_File, Ada.Text_IO.In_File,
                        Petal.Actual_Arguments.To_String (Options));

      while not Ada.Text_IO.End_Of_File (Options_File) loop
         declare

            procedure Interpret_Line
              (Short_Name : String;
               Long_Name  : String;
               Property   : String;
               Type_Name  : String;
               Key        : String);

            --------------------
            -- Interpret_Line --
            --------------------

            procedure Interpret_Line
              (Short_Name : String;
               Long_Name  : String;
               Property   : String;
               Type_Name  : String;
               Key        : String)
            is
               use Petal.Formal_Arguments;
               Formal     : Petal.Formal_Arguments.Formal_Argument;
               Value_Type : constant Petal.Values.Value_Type :=
                              (if Type_Name = "" or else Type_Name = "boolean"
                               then Petal.Values.Boolean_Value
                               elsif Type_Name = "integer"
                               then Petal.Values.Integer_Value
                               elsif Type_Name = "string"
                               then Petal.Values.String_Value
                               else Petal.Values.String_Value);
               Arg_Type   : constant Argument_Type :=
                              (if Type_Name = "<"
                               then Read_Stream_Argument
                               elsif Type_Name = ">"
                               then Write_Stream_Argument
                               else Value_Argument);
            begin
               if Short_Name = ""
                 and then Long_Name = ""
               then
                  declare
                     Required : constant Boolean :=
                                  Property /= ""
                                      and then Property /= "*";
                     Multiple : constant Boolean :=
                                  Property = "+"
                                      or else Property = "*";
                  begin
                     Formal :=
                       Petal.Formal_Arguments.Positional_Argument
                         (Key           => Key,
                          Required      => Required,
                          Multiple      => Multiple,
                          Value_Type    => Value_Type,
                          Expected_Type => Arg_Type);
                  end;
               else
                  null;
               end if;

               Formal_Count := Formal_Count + 1;
               Formals (Formal_Count) := Formal;

            end Interpret_Line;

            Line : constant String :=
                     Ada.Text_IO.Get_Line (Options_File);
         begin
            Parse_Line (Line, Interpret_Line'Access);
            exit when Formal_Count = Max_Formals;
         end;
      end loop;

      Ada.Text_IO.Close (Options_File);

      declare
         Image_Stream : constant Rose.Capabilities.Capability :=
                          Petal.Actual_Arguments.To_Stream_Reader
                            (Image);
         Launch_Cap   : constant Rose.Capabilities.Capability :=
                          Rose.System_Calls.Client.Get_Capability
                            (Install_Cap,
                             Caps => (1 => Image_Stream));
         Command      : constant Petal_Command :=
                          Create_With_Launch_Cap
                            (Launch_Cap, Formals (1 .. Formal_Count));
      begin
         Petal.Exec.Install
           (Name    => Petal.Actual_Arguments.To_String (Command_Name),
            Command => Command);

         return Petal.Values.To_Value (True);
      end;
   end On_Install;

end Petal.Commands.Install;
