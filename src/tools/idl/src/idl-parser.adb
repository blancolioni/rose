with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with IDL.Parser.Tokens;                 use IDL.Parser.Tokens;
with IDL.Parser.Lexical;                use IDL.Parser.Lexical;

with IDL.Interface_Table;
with IDL.Types;

package body IDL.Parser is

   Current_Interface : IDL.Syntax.IDL_Interface;
   Current_Directory : Ada.Strings.Unbounded.Unbounded_String;

   function Parse_Interface return IDL.Syntax.IDL_Interface;
   function Parse_Procedure_Declaration return IDL.Procs.IDL_Procedure;

   procedure Parse_Declaration
     (For_Interface : IDL.Syntax.IDL_Interface);

   procedure Parse_Object_Decl
     (For_Interface : IDL.Syntax.IDL_Interface);

   procedure Parse_Type_Decl
     (For_Interface : IDL.Syntax.IDL_Interface);

   procedure Parse_Subprogram
     (For_Interface : IDL.Syntax.IDL_Interface);

   procedure Parse_Arguments
     (For_Subprogram : IDL.Syntax.IDL_Subprogram);

   function Parse_Type
     return IDL.Types.IDL_Type;

   function At_Declaration_Start return Boolean;

   --------------------------
   -- At_Declaration_Start --
   --------------------------

   function At_Declaration_Start return Boolean is
   begin
      case Tok is
         when Tok_Function | Tok_Identifier |
              Tok_Procedure | Tok_Type =>
            return True;
         when others =>
            return False;
      end case;
   end At_Declaration_Start;

   ---------------------
   -- Parse_Arguments --
   ---------------------

   procedure Parse_Arguments
     (For_Subprogram : IDL.Syntax.IDL_Subprogram)
   is
   begin
      loop
         if Tok /= Tok_Identifier then
            Error ("missing argument name");
            raise Parse_Error;
         end if;

         declare
            Arg_Name : constant String := Tok_Text;
            Mode     : IDL.Syntax.IDL_Argument_Mode :=
              IDL.Syntax.In_Argument;
            Arg_Type : IDL.Types.IDL_Type;
         begin

            Scan;

            if Tok /= Tok_Colon then
               Error ("missing ':'");
               raise Parse_Error;
            end if;

            Scan;

            if Tok = Tok_In then
               Scan;
               if Tok = Tok_Out then
                  Scan;
                  Mode := IDL.Syntax.Inout_Argument;
               else
                  Mode := IDL.Syntax.In_Argument;
               end if;
            elsif Tok = Tok_Out then
               Scan;
               Mode := IDL.Syntax.Out_Argument;
            end if;

            Arg_Type := Parse_Type;

            IDL.Syntax.Add_Argument
              (For_Subprogram, Arg_Name, Arg_Type, Mode);

            if Tok = Tok_Semi then
               Scan;
            else
               exit;
            end if;

         end;
      end loop;

   end Parse_Arguments;

   -----------------------
   -- Parse_Declaration --
   -----------------------

   procedure Parse_Declaration
     (For_Interface : IDL.Syntax.IDL_Interface)
   is
   begin
      pragma Assert (At_Declaration_Start);

      case Tok is
         when Tok_Function | Tok_Procedure =>
            Parse_Subprogram (For_Interface);
         when Tok_Type =>
            Parse_Type_Decl (For_Interface);
         when Tok_Identifier =>
            Parse_Object_Decl (For_Interface);
         when others =>
            raise Program_Error;
      end case;
   end Parse_Declaration;

   ---------------------
   -- Parse_Interface --
   ---------------------

   function Parse_Interface return IDL.Syntax.IDL_Interface is
      Result : IDL.Syntax.IDL_Interface;
   begin

      if Tok /= Tok_Interface then
         Error ("missing 'interface'");
         raise Parse_Error;
      end if;

      Scan;

      if Tok /= Tok_Identifier then
         Error ("missing interface name");
         raise Parse_Error;
      end if;

      if IDL.Interface_Table.Contains (Tok_Text) then
         return IDL.Interface_Table.Element (Tok_Text);
      end if;

      Result := IDL.Syntax.New_Interface (Tok_Text);
      Current_Interface := Result;
      IDL.Interface_Table.Insert (Result);

      Scan;

      if Tok = Tok_Left_Paren then
         Scan;
         if Tok /= Tok_Identifier then
            Error ("missing constraint");
         else
            while Tok = Tok_Identifier loop
               declare
                  Name : constant String := Tok_Text;
                  Constraint_Type : IDL.Types.IDL_Type;
               begin
                  Scan;
                  if Tok = Tok_Colon then
                     Scan;
                  else
                     Error ("missing ':'");
                  end if;
                  Constraint_Type := Parse_Type;
                  IDL.Syntax.Add_Constraint
                    (Result, Name, Constraint_Type);
               end;
               if Tok = Tok_Comma then
                  Scan;
                  if Tok /= Tok_Identifier then
                     if Tok = Tok_Right_Paren then
                        Error ("extra ',' ignored");
                     else
                        Error ("syntax error");
                     end if;
                  end if;
               elsif Tok = Tok_Identifier then
                  Error ("missing ','");
               end if;
            end loop;
         end if;

         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')'");
         end if;

      end if;

      if Tok = Tok_Colon then
         Scan;

         loop

            if Tok /= Tok_Identifier then
               Error ("missing inherited interface name");
               raise Parse_Error;
            end if;

            declare
               Parent : constant IDL.Syntax.IDL_Interface :=
                          Parse_Interface_File (Tok_Text & ".idl");
            begin
               IDL.Syntax.Add_Inherited (Result, Parent);
            end;
            Scan;
            if Tok = Tok_Comma then
               Scan;
            else
               exit;
            end if;
         end loop;
      end if;

      if Tok = Tok_With then
         Scan;
         while Tok = Tok_Identifier loop
            declare
               Aspect_Name : constant String := Tok_Text;
            begin
               Scan;
               if Tok = Tok_Arrow then
                  Scan;
               else
                  Error ("missing '=>'");
               end if;
               if Tok = Tok_Integer_Constant then
                  IDL.Syntax.Add_Aspect
                    (Result, Aspect_Name, Natural'Value (Tok_Text));
                  Scan;
               else
                  Error ("bad aspect value");
                  while Tok /= Tok_Comma
                    and then Tok /= Tok_Semi
                    and then Tok /= Tok_Is
                  loop
                     Scan;
                  end loop;
               end if;
               if Tok = Tok_Comma then
                  Scan;
                  if Tok = Tok_Is then
                     Error ("extra ',' ignored");
                     exit;
                  elsif Tok /= Tok_Identifier then
                     Error ("missing aspect");
                     exit;
                  end if;
               else
                  exit;
               end if;
            end;
         end loop;
      end if;

      if Tok /= Tok_Is then
         Error ("missing 'is'");
         raise Parse_Error;
      end if;

      Scan;

      while At_Declaration_Start loop
         Parse_Declaration (Result);
      end loop;

      if Tok /= Tok_End then
         Error ("missing 'end'");
         raise Parse_Error;
      end if;

      Scan;

      if Tok /= Tok_Identifier then
         Error ("missing '" & IDL.Syntax.Get_Name (Result) & "'");
         raise Parse_Error;
      end if;

      Scan;

      if Tok /= Tok_Semi then
         Error ("missing ';'");
         raise Parse_Error;
      end if;

      return Result;

   end Parse_Interface;

   --------------------------
   -- Parse_Interface_File --
   --------------------------

   function Parse_Interface_File
     (Path : String)
      return IDL.Syntax.IDL_Interface
   is
      use Ada.Strings.Unbounded;
      Result : IDL.Syntax.IDL_Interface;
   begin
      if Ada.Directories.Exists (Path) then
         Open (Path);
         if Current_Directory = Null_Unbounded_String then
            Current_Directory :=
              To_Unbounded_String
                (Ada.Directories.Containing_Directory (Path));
         end if;
      else
         declare
            New_Path : constant String :=
                         Ada.Directories.Compose
                           (To_String (Current_Directory), Path);
         begin
            if Ada.Directories.Exists (New_Path) then
               Open (New_Path);
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Cannot open interface file '" & Path & "'");
               return IDL.Syntax.Null_Interface;
            end if;
         end;
      end if;

      Result := Parse_Interface;
      Close;
      return Result;
   end Parse_Interface_File;

   -----------------------
   -- Parse_Object_Decl --
   -----------------------

   procedure Parse_Object_Decl
     (For_Interface : IDL.Syntax.IDL_Interface)
   is
      Name     : constant String := Tok_Text;
   begin
      pragma Assert (Tok = Tok_Identifier);
      Scan;
      if Tok /= Tok_Colon then
         if Tok = Tok_Comma then
            Error ("only one object per declaration supported");
         else
            Error ("missing ':'");
         end if;
         raise Parse_Error;
      end if;

      Scan;

      if Tok = Tok_Identifier then
         Error ("only constants supported in object declarations");
         raise Parse_Error;
      end if;

      if Tok = Tok_Constant then
         Scan;
      end if;

      declare
         Object_Type : IDL.Types.IDL_Type := IDL.Types.No_Type;
      begin

         if Tok /= Tok_Becomes then
            Object_Type := Parse_Type;
         end if;

         if Tok /= Tok_Becomes then
            Error ("missing initialiser");
            raise Parse_Error;
         end if;

         Scan;

         if Tok /= Tok_Identifier
           and then Tok /= Tok_Integer_Constant
         then
            Error ("missing value (must be integer or identifier)");
            raise Parse_Error;
         end if;

         IDL.Syntax.Add_Constant (For_Interface,
                                  Name, Object_Type, Tok_Text);

         Scan;

         if Tok /= Tok_Semi then
            Error ("missing ';'");
            raise Parse_Error;
         end if;

         Scan;

      end;

   end Parse_Object_Decl;

   ----------------------
   -- Parse_Petal_File --
   ----------------------

   function Parse_Petal_File
     (Path : String)
      return IDL.Procs.IDL_Procedure
   is
      Result : IDL.Procs.IDL_Procedure;
   begin
      Open (Path);
      Result := Parse_Procedure_Declaration;
      Close;
      return Result;
   end Parse_Petal_File;

   ---------------------------------
   -- Parse_Procedure_Declaration --
   ---------------------------------

   function Parse_Procedure_Declaration return IDL.Procs.IDL_Procedure is
      Proc : IDL.Procs.IDL_Procedure;

      procedure Parse_Declarations
        (Closing_Semi : Boolean;
         Add          : not null access
           procedure (Proc : IDL.Procs.IDL_Procedure;
                      Name : String;
                      Typ  : IDL.Types.IDL_Type));

      procedure Parse_Arguments;
      procedure Parse_Locals;
      procedure Parse_Commands;
      procedure Parse_Command_Aspect (Command : IDL.Procs.IDL_Command);

      function Parse_Expression return IDL.Procs.IDL_Expression;

      ---------------------
      -- Parse_Arguments --
      ---------------------

      procedure Parse_Arguments is
      begin
         pragma Assert (Tok = Tok_Left_Paren);
         Scan;

         Parse_Declarations
           (False, IDL.Procs.Add_Argument'Access);

         if Tok = Tok_Right_Paren then
            Scan;
         end if;
      end Parse_Arguments;

      --------------------------
      -- Parse_Command_Aspect --
      --------------------------

      procedure Parse_Command_Aspect (Command : IDL.Procs.IDL_Command) is
         Aspect_Name : constant String := Tok_Text;
         Aspect_Value : IDL.Procs.IDL_Expression;
      begin
         Scan;
         if Tok /= Tok_Arrow then
            Error ("missing '=>'");
            raise Parse_Error;
         end if;

         Scan;

         Aspect_Value := Parse_Expression;

         IDL.Procs.Add_Aspect (Command, Aspect_Name, Aspect_Value);
      end Parse_Command_Aspect;

      --------------------
      -- Parse_Commands --
      --------------------

      procedure Parse_Commands is
      begin
         while Tok = Tok_Invoke loop
            Scan;

            if Tok /= Tok_Identifier then
               Error ("missing cap name");
               raise Parse_Error;
            end if;

            declare
               Cap_Name : constant String := Tok_Text;
               Endpoint : Positive := 1;
            begin
               Scan;
               if Tok = Tok_Forward_Slash then
                  Scan;
                  if Tok = Tok_Integer_Constant then
                     Endpoint := Positive'Value (Tok_Text);
                     Scan;
                  else
                     Error ("expected an integer literal");
                     raise Parse_Error;
                  end if;
               end if;

               declare
                  Command : constant IDL.Procs.IDL_Command :=
                              IDL.Procs.New_Invoke_Command
                                (Cap_Name => Cap_Name,
                                 Endpoint => Endpoint);
               begin
                  if Tok = Tok_With then
                     Scan;
                     while Tok = Tok_Identifier loop
                        Parse_Command_Aspect (Command);
                        if Tok = Tok_Comma then
                           Scan;
                           if Tok /= Tok_Identifier then
                              Error ("expected an aspect name");
                              raise Parse_Error;
                           end if;
                        elsif Tok = Tok_Identifier then
                           Error ("missing ','");
                        end if;
                     end loop;
                  end if;

                  IDL.Procs.Add_Command (Proc, Command);

               end;

               if Tok = Tok_Semi then
                  Scan;
               elsif Tok = Tok_Invoke then
                  Error ("missing ';'");
               end if;
            end;
         end loop;
      end Parse_Commands;

      ------------------------
      -- Parse_Declarations --
      ------------------------

      procedure Parse_Declarations
        (Closing_Semi : Boolean;
         Add          : not null access
           procedure (Proc : IDL.Procs.IDL_Procedure;
                      Name : String;
                      Typ  : IDL.Types.IDL_Type))
      is
      begin
         while Tok = Tok_Identifier loop
            declare
               Name     : constant String := Tok_Raw_Text;
               Arg_Type : IDL.Types.IDL_Type;
            begin
               Scan;
               if Tok = Tok_Colon then
                  Scan;
               else
                  Error ("missing ':'");
                  raise Parse_Error;
               end if;

               Arg_Type := Parse_Type;

               Add (Proc, Name, Arg_Type);

               if Tok = Tok_Semi then
                  Scan;
               elsif Tok = Tok_Identifier and then
                 Next_Tok = Tok_Colon
               then
                  Error ("missing ';'");
               elsif Closing_Semi then
                  Error ("missing ';'");
                  raise Parse_Error;
               end if;
            end;
         end loop;
      end Parse_Declarations;

      ----------------------
      -- Parse_Expression --
      ----------------------

      function Parse_Expression return IDL.Procs.IDL_Expression is
         Expr : IDL.Procs.IDL_Expression;
      begin
         if Tok = Tok_Left_Paren then
            Expr := IDL.Procs.New_List_Expression;

            Scan;
            while Tok /= Tok_End_Of_File
              and then Tok /= Tok_Right_Paren
            loop
               IDL.Procs.Append (Expr, Parse_Expression);
               if Tok = Tok_Comma then
                  Scan;
               elsif Tok /= Tok_Right_Paren then
                  Error ("missing ')'");
                  raise Parse_Error;
               end if;
            end loop;

            if Tok = Tok_Right_Paren then
               Scan;
            end if;

         elsif Tok = Tok_Integer_Constant then
            Expr :=
              IDL.Procs.New_Integer_Expression
                (Integer'Value (Tok_Text));
            Scan;
         elsif Tok = Tok_String_Constant then
            Expr :=
              IDL.Procs.New_String_Expression (Tok_Text);
            Scan;
         elsif Tok = Tok_Identifier then
            Expr :=
              IDL.Procs.New_Object_Expression (Tok_Raw_Text);
            Scan;
         else
            Error ("expected an expression");
            raise Parse_Error;
         end if;

         if Tok = Tok_Ampersand then
            Scan;
            Expr :=
              IDL.Procs.New_Operator_Expression
                ("&", Expr, Parse_Expression);
         end if;

         return Expr;
      end Parse_Expression;

      ------------------
      -- Parse_Locals --
      ------------------

      procedure Parse_Locals is
      begin
         Parse_Declarations
           (True, IDL.Procs.Add_Local'Access);
      end Parse_Locals;

   begin
      if Tok = Tok_Procedure
        and then Next_Tok = Tok_Identifier
      then
         Scan;
      else
         Error ("missing procedure declaration");
         raise Parse_Error;
      end if;

      Proc := IDL.Procs.New_Procedure_Declaration (Tok_Text);
      Scan;

      if Tok = Tok_Left_Paren then
         Parse_Arguments;
      end if;

      if Tok = Tok_Is then
         Scan;
      else
         Error ("missing 'is'");
      end if;

      Parse_Locals;

      if Tok = Tok_Begin then
         Scan;
      else
         Error ("missing 'begin'");
      end if;

      Parse_Commands;

      if Tok = Tok_End then
         Scan;
         if Tok = Tok_Identifier then
            if Tok_Text /= IDL.Procs.Declared_Name (Proc) then
               Error ("name does not match (should be '"
                      & IDL.Procs.Declared_Name (Proc) & "'");
            end if;
            Scan;
         end if;

         if Tok = Tok_Semi then
            Scan;
            if Tok /= Tok_End_Of_File then
               Error ("extra tokens ignored");
            end if;
         else
            Error ("missing ';'");
         end if;
      else
         Error ("missing 'end'");
      end if;

      return Proc;

   end Parse_Procedure_Declaration;

   ----------------------
   -- Parse_Subprogram --
   ----------------------

   procedure Parse_Subprogram
     (For_Interface : IDL.Syntax.IDL_Interface)
   is
      Is_Function : constant Boolean := Tok = Tok_Function;
      Result      : IDL.Syntax.IDL_Subprogram;
   begin
      Scan;

      if Tok /= Tok_Identifier then
         Error ("missing identifier");
         raise Parse_Error;
      end if;

      Result := IDL.Syntax.New_Subprogram (For_Interface, Tok_Text);
      Scan;

      if Tok = Tok_Left_Paren then
         Scan;
         Parse_Arguments (Result);

         if Tok /= Tok_Right_Paren then
            Error ("missing ')' at " & Token'Image (Tok));
            raise Parse_Error;
         end if;

         Scan;
      end if;

      if Tok = Tok_Return then
         if not Is_Function then
            Error ("procedures cannot return values (use function)");
         end if;

         Scan;

         IDL.Syntax.Set_Result_Type (Result, Parse_Type);

      else
         if Is_Function then
            Error ("missing return type");
         end if;
      end if;

      if Tok = Tok_With then
         Scan;
         while Tok = Tok_Identifier loop
            declare
               Aspect_Name : constant String := Tok_Text;
            begin
               Scan;
               if Tok = Tok_Apostrophe then
                  Scan;
                  if Tok = Tok_Identifier then
                     declare
                        Attribute : constant String :=
                                      Tok_Text;
                     begin
                        Scan;
                        if Tok = Tok_Arrow then
                           Scan;
                        else
                           Error ("missing '=>'");
                        end if;
                        if Tok = Tok_Identifier then
                           IDL.Syntax.Add_Aspect
                             (Result, Aspect_Name, Attribute, Tok_Text);
                           Scan;
                        else
                           Error ("bad aspect value");
                           while Tok /= Tok_Comma
                             and then Tok /= Tok_Semi
                             and then Tok /= Tok_Is
                           loop
                              Scan;
                           end loop;
                        end if;
                     end;
                  else
                     Error ("missing identifier");
                  end if;
               else
                  if Tok = Tok_Arrow then
                     Scan;
                  else
                     Error ("missing '=>'");
                  end if;
                  if Tok = Tok_Identifier then
                     IDL.Syntax.Add_Aspect
                       (Result, Aspect_Name, "", Tok_Text);
                     Scan;
                  else
                     Error ("bad aspect value");
                     while Tok /= Tok_Comma
                       and then Tok /= Tok_Semi
                       and then Tok /= Tok_Is
                     loop
                        Scan;
                     end loop;
                  end if;
               end if;
               if Tok = Tok_Comma then
                  Scan;
                  if Tok = Tok_Is then
                     Error ("extra ',' ignored");
                     exit;
                  elsif Tok /= Tok_Identifier then
                     Error ("missing aspect");
                     exit;
                  end if;
               else
                  exit;
               end if;
            end;
         end loop;
      end if;

      if Tok = Tok_Semi then
         Scan;
      else
         Error ("missing ';'");
      end if;

   end Parse_Subprogram;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type
     return IDL.Types.IDL_Type
   is
      use type IDL.Types.IDL_Type;
      Result_Type : IDL.Types.IDL_Type;
      Is_Interface : constant Boolean := Tok = Tok_Interface;
   begin

      if Tok = Tok_Record then
         Scan;
         Result_Type := IDL.Types.New_Record_Type;
         while Tok = Tok_Identifier loop
            declare
               Field_Name : constant String := Tok_Text;
               Field_Type : IDL.Types.IDL_Type;
            begin
               Scan;
               if Tok = Tok_Colon then
                  Scan;
               else
                  Error ("missing ':'");
               end if;

               Field_Type := Parse_Type;
               if Field_Type /= IDL.Types.No_Type then
                  IDL.Types.Add_Field (Result_Type, Field_Name, Field_Type);
               end if;
               if Tok = Tok_Semi then
                  Scan;
               else
                  Error ("missing ';'");
               end if;
            end;
         end loop;

         if Tok = Tok_End then
            Scan;
            if Tok = Tok_Record then
               Scan;
            else
               Error ("missing 'end record'");
            end if;
         else
            Error ("missing 'end record'");
         end if;

         return Result_Type;
      end if;

      if Tok = Tok_Left_Paren then
         Scan;
         Result_Type :=
           IDL.Types.New_Enumerated_Type
             (IDL.Syntax.Get_Ada_Name (Current_Interface));

         while Tok = Tok_Identifier loop
            IDL.Types.Add_Literal (Result_Type, Tok_Text);
            Scan;
            if Tok = Tok_Comma then
               Scan;
            elsif Tok = Tok_Identifier then
               Error ("missing ','");
            else
               exit;
            end if;
         end loop;

         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')'");
         end if;

         return Result_Type;
      end if;

      if Tok = Tok_Interface then
         Scan;
      end if;

      if Tok /= Tok_Identifier then
         Error ("missing type");
         raise Parse_Error;
      end if;

      if Is_Interface then
         IDL.Types.Add_Interface_Type (Tok_Text);
      end if;

      Result_Type := IDL.Types.Get_Type (Tok_Text);

      if Result_Type = IDL.Types.No_Type then
         Error (Tok_Raw_Text & ": no such type");
         raise Parse_Error;
      end if;

      Scan;

      return Result_Type;

   end Parse_Type;

   ---------------------
   -- Parse_Type_Decl --
   ---------------------

   procedure Parse_Type_Decl
     (For_Interface : IDL.Syntax.IDL_Interface)
   is
   begin
      pragma Assert (Tok = Tok_Type);
      Scan;

      if Tok /= Tok_Identifier then
         Error ("missing type name");
         raise Parse_Error;
      end if;

      declare
         Name : constant String := Tok_Text;
         Item_Type : IDL.Types.IDL_Type;
         Derived   : Boolean := False;
      begin
         Scan;

         if Tok /= Tok_Is then
            Error ("missing 'is'");
            raise Parse_Error;
         end if;

         Scan;

         if Tok = Tok_Range then
            Scan;
            declare
               Low, High : Integer;
            begin
               if Tok = Tok_Integer_Constant then
                  Low := Integer'Value (Tok_Text);
                  Scan;
               else
                  Error ("expected an integer constant");
                  raise Parse_Error;
               end if;
               if Tok = Tok_Dot_Dot then
                  Scan;
               else
                  Error ("expected '..'");
                  raise Parse_Error;
               end if;
               if Tok = Tok_Integer_Constant then
                  High := Integer'Value (Tok_Text);
                  Scan;
               else
                  Error ("expected an integer constant");
                  raise Parse_Error;
               end if;

               if Tok /= Tok_Semi then
                  Error ("missing ';'");
                  raise Parse_Error;
               end if;

               Scan;

               Item_Type :=
                 IDL.Types.New_Range_Type
                   (IDL.Syntax.Get_Ada_Name (For_Interface),
                    Low, High);
               IDL.Syntax.Add_Type (For_Interface, Name, Item_Type);
            end;
         else

            if Tok = Tok_New then
               Derived := True;
               Scan;
            end if;

            Item_Type := Parse_Type;

            if Tok /= Tok_Semi then
               Error ("missing ';'");
               raise Parse_Error;
            end if;

            Scan;

            if Derived then
               IDL.Syntax.Add_Derived_Type (For_Interface, Name, Item_Type);
            else
               IDL.Syntax.Add_Type (For_Interface, Name, Item_Type);
            end if;
         end if;
      end;
   end Parse_Type_Decl;

end IDL.Parser;
