with Ada.Text_IO;

with Petal.Exec;

package body Petal.Parser is

   type Token is (Tok_Bad_Character, Tok_Identifier,
                  Tok_Ampersand, Tok_Semicolon,
                  Tok_End_Of_Line);

   ---------------
   -- Arguments --
   ---------------

   function Arguments
     (Element : Parse_Element)
      return Petal.Actual_Arguments.Actual_Argument_Container
   is
   begin
      return Element.Arguments;
   end Arguments;

   -------------
   -- Command --
   -------------

   function Command
     (Element : Parse_Element) return Petal.Commands.Petal_Command
   is
   begin
      return Element.Command;
   end Command;

   -------------
   -- Context --
   -------------

   function Context
     (Element : Parse_Element) return Petal.Commands.Launch_Context
   is
   begin
      return Element.Context;
   end Context;

   -------------
   -- Element --
   -------------

   function Element (Result : Parse_Result) return Parse_Element is
   begin
      return Result.Elements (Result.Current);
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Result : Parse_Result) return Boolean is
   begin
      return Result.Current <= Result.Count;
   end Has_Element;

   ----------------
   -- Is_Command --
   ----------------

   function Is_Command (Element : Parse_Element) return Boolean is
   begin
      return Element.Is_Command;
   end Is_Command;

   ----------
   -- Next --
   ----------

   procedure Next (Result : in out Parse_Result) is
   begin
      Result.Current := Result.Current + 1;
   end Next;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Line   : String;
      Result : out Parse_Result)
   is
      Tok          : Token := Tok_Bad_Character;
      Current_Text : String (1 .. 100) := (others => ' ');
      Text_Length  : Natural := 0;
      Index        : Positive := Line'First;

      function Current_Char return Character
      is (if Index <= Line'Last then Line (Index) else ' ');

      function End_Of_Line return Boolean
      is (Index > Line'Last);

      function Tok_Text return String
      is (Current_Text (1 .. Text_Length));

      procedure Next_Char;

      procedure Error (Message : String);
      procedure Scan;

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Output,
            Message);
         Result.Count := 0;
      end Error;

      ---------------
      -- Next_Char --
      ---------------

      procedure Next_Char is
      begin
         Index := Index + 1;
      end Next_Char;

      ----------
      -- Scan --
      ----------

      procedure Scan is
      begin
         while not End_Of_Line
           and then Current_Char = ' '
         loop
            Next_Char;
         end loop;

         if End_Of_Line then
            Tok := Tok_End_Of_Line;
            return;
         end if;

         Current_Text (1) := Current_Char;
         Text_Length := 1;

         case Current_Char is
            when ';' =>
               Tok := Tok_Semicolon;
            when '&' =>
               Tok := Tok_Ampersand;
            when others =>
               Next_Char;
               while not End_Of_Line
                 and then Current_Char /= ' '
                 and then Current_Char not in ';' | '&'
               loop
                  Text_Length := Text_Length + 1;
                  Current_Text (Text_Length) := Current_Char;
                  Next_Char;
               end loop;
               Tok := Tok_Identifier;
         end case;
      end Scan;

   begin
      Result.Count := 0;
      Result.Current := 1;
      Scan;

      while Tok /= Tok_End_Of_Line loop

         if Tok = Tok_Identifier then

            declare
               Element       : Parse_Element;
               Named_Enabled : Boolean := True;

               procedure Add_Long_Argument
                 (Name_And_Value : String);

               procedure Add_Short_Argument
                 (Name : Character);

               -----------------------
               -- Add_Long_Argument --
               -----------------------

               procedure Add_Long_Argument
                 (Name_And_Value : String)
               is
               begin
                  for I in Name_And_Value'Range loop
                     if Name_And_Value (I) = '=' then
                        declare
                           Name : constant String :=
                                    Name_And_Value
                                      (Name_And_Value'First .. I - 1);
                           Value : constant String :=
                                     Name_And_Value
                                       (I + 1 .. Name_And_Value'Last);
                        begin
                           Petal.Actual_Arguments.Add_Argument
                             (Element.Arguments,
                              Petal.Actual_Arguments.To_Actual_Argument
                                (Petal.Commands.Formal_Argument
                                     (Element.Command, Name),
                                 Value));
                           return;
                        end;
                     end if;
                  end loop;
                  Petal.Actual_Arguments.Add_Argument
                    (Element.Arguments,
                     Petal.Actual_Arguments.To_Actual_Argument
                       (Petal.Commands.Formal_Argument
                            (Element.Command,
                             Name_And_Value),
                        "true"));
               end Add_Long_Argument;

               ------------------------
               -- Add_Short_Argument --
               ------------------------

               procedure Add_Short_Argument
                 (Name : Character)
               is
               begin
                  Petal.Actual_Arguments.Add_Argument
                    (Element.Arguments,
                     Petal.Actual_Arguments.To_Actual_Argument
                       (Petal.Commands.Formal_Argument
                            (Element.Command, Name),
                        "true"));
               end Add_Short_Argument;

            begin
               Element.Command := Petal.Exec.Find (Tok_Text);
               Element.Context := Petal.Commands.Default_Launch_Context;

               Scan;

               while Tok = Tok_Identifier loop

                  declare
                     Name : constant String := Tok_Text;
                  begin
                     if Name = "--" then
                        Named_Enabled := False;
                     elsif Named_Enabled
                       and then Name'Length > 2
                       and then Name (Name'First) = '-'
                       and then Name (Name'First + 1) = '-'
                     then
                        Add_Long_Argument
                          (Name (Name'First + 2 .. Name'Last));
                     elsif Named_Enabled
                       and then Name (Name'First) = '-'
                     then
                        for Ch of Name (Name'First + 1 .. Name'Last) loop
                           Add_Short_Argument (Ch);
                        end loop;
                     else
                        Petal.Actual_Arguments.Add_Argument
                          (Element.Arguments,
                           Petal.Actual_Arguments.To_Actual_Argument
                             (Petal.Commands.Extra_Arguments (Element.Command),
                              Tok_Text));
                     end if;
                  end;
                  Scan;
               end loop;

               Result.Count := Result.Count + 1;
               Result.Elements (Result.Count) := Element;
            end;
         elsif Tok = Tok_Semicolon then
            Scan;
         else
            Error ("missing command at '" & Tok_Text & "'");
            return;
         end if;
      end loop;
   end Parse;

end Petal.Parser;
