with IDL.Identifiers;
with IDL.Types;

with Syn.Blocks;
with Syn.Declarations;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;
with Syn.File_Writer;

package body IDL.Generate is

   function Get_Syn_Mode
     (Arg : IDL.Syntax.IDL_Argument)
      return Syn.Declarations.Argument_Mode
   is (case IDL.Syntax.Get_Mode (Arg) is
          when IDL.Syntax.In_Argument => Syn.Declarations.In_Argument,
          when IDL.Syntax.Out_Argument => Syn.Declarations.Out_Argument,
          when IDL.Syntax.Inout_Argument => Syn.Declarations.Inout_Argument);

   procedure Generate_Interface_Package (Item : IDL.Syntax.IDL_Interface);
   procedure Generate_Client_Package (Item : IDL.Syntax.IDL_Interface);
   procedure Generate_Server_Package (Item : IDL.Syntax.IDL_Interface);

   procedure Generate_Inherited_Overrides
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Parent  : IDL.Syntax.IDL_Interface);

   procedure Generate_Client_Override
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Index   : Positive;
      Parent  : IDL.Syntax.IDL_Interface);

   procedure Generate_Interface_Record
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Client  : Boolean);

   procedure Initialise_Invocation
     (Block : in out Syn.Blocks.Block_Type'Class;
      Subpr : IDL.Syntax.IDL_Subprogram;
      Index : Positive);

   procedure Copy_Invocation_Result
     (Block : in out Syn.Blocks.Block_Type'Class;
      Subpr : IDL.Syntax.IDL_Subprogram);

   procedure Copy_Server_Invocation
     (Block       : in out Syn.Blocks.Block_Type'Class;
      Server_Type : String;
      Subpr       : IDL.Syntax.IDL_Subprogram);

   procedure With_Packages
     (Top_Interface : IDL.Syntax.IDL_Interface;
      Pkg           : in out Syn.Declarations.Package_Type'Class;
      Server        : Boolean);

   ----------------------------
   -- Copy_Invocation_Result --
   ----------------------------

   procedure Copy_Invocation_Result
     (Block : in out Syn.Blocks.Block_Type'Class;
      Subpr : IDL.Syntax.IDL_Subprogram)
   is
      use IDL.Syntax, IDL.Types;
      Args       : constant IDL_Argument_Array := Get_Arguments (Subpr);
      Recv_Count : Natural := 0;

      procedure Copy_Array_Type
        (Copy_Block : in out Syn.Blocks.Block_Type'Class;
         Base_Name  : String;
         Count_Name : String;
         Base_Type  : IDL_Type);

      procedure Copy_Scalar_Type
        (Copy_Block : in out Syn.Blocks.Block_Type'Class;
         Base_Name  : String;
         Base_Type  : IDL_Type);

      ---------------------
      -- Copy_Array_Type --
      ---------------------

      procedure Copy_Array_Type
        (Copy_Block : in out Syn.Blocks.Block_Type'Class;
         Base_Name  : String;
         Count_Name : String;
         Base_Type  : IDL_Type)
      is
         pragma Unreferenced (Base_Type);
         Call : Syn.Statements.Procedure_Call_Statement'Class :=
                  Syn.Statements.New_Procedure_Call_Statement
                    ("Copy_From_Memory_Cap");
      begin
         Call.Add_Actual_Argument
           ("Item." & Get_Ada_Name (Subpr) & "_" & Base_Name & "_Cap");
         if Count_Name /= "" then
            Call.Add_Actual_Argument
              (Syn.Constrained_Array
                 (Base_Name, Syn.Literal (1), Syn.Object (Count_Name)));
         else
            Call.Add_Actual_Argument
              (Syn.Object (Base_Name));
         end if;

         Copy_Block.Append (Call);
      end Copy_Array_Type;

      ----------------------
      -- Copy_Scalar_Type --
      ----------------------

      procedure Copy_Scalar_Type
        (Copy_Block : in out Syn.Blocks.Block_Type'Class;
         Base_Name  : String;
         Base_Type  : IDL_Type)
      is
      begin
         if Is_Record_Type (Base_Type) then
            for F in 1 .. Get_Field_Count (Base_Type) loop
               Recv_Count := Recv_Count + 1;
               Copy_Block.Append
                 (Syn.Statements.New_Assignment_Statement
                    (Base_Name & "."
                     & IDL.Identifiers.To_Ada_Name
                       (Get_Field_Name (Base_Type, F)),
                     Syn.Expressions.New_Function_Call_Expression
                       ("Get_Data_Word",
                        Syn.Object ("Params"),
                        Syn.Literal (Recv_Count))));
            end loop;
         elsif not Is_Scalar (Base_Type) then
            null;
         elsif Is_Enumerated_Type (Base_Type) then
            Recv_Count := Recv_Count + 1;
            Copy_Block.Append
              (Syn.Statements.New_Assignment_Statement
                 (Base_Name,
                  Syn.Expressions.New_Function_Call_Expression
                    (IDL.Types.Get_Ada_Name (Base_Type) & "'Val",
                     Syn.Expressions.New_Function_Call_Expression
                       ("Rose.Words.Word'",
                        Syn.Expressions.New_Function_Call_Expression
                          ("Get_Data_Word",
                           Syn.Object ("Params"),
                           Syn.Literal (Recv_Count))))));
         elsif Is_Interface (Base_Type) then
            Recv_Count := Recv_Count + 1;
            Copy_Block.Append
              (Syn.Statements.New_Assignment_Statement
                 (Base_Name,
                  Syn.Expressions.New_Function_Call_Expression
                    (Get_Ada_Package (Base_Type) & ".Client.Create_Client",
                     Syn.Expressions.New_Function_Call_Expression
                       ("Get_Data_Word",
                        Syn.Object ("Params"),
                        Syn.Literal (Recv_Count)))));
         elsif Get_Name (Base_Type) = "stream_element_count"
           or else Get_Name (Base_Type) = "stream_element_offset"
         then
            Recv_Count := Recv_Count + 1;
            Copy_Block.Append
              (Syn.Statements.New_Assignment_Statement
                 (Base_Name,
                  Syn.Expressions.New_Function_Call_Expression
                    ("Get_Data_Word",
                     Syn.Object ("Params"),
                     Syn.Literal (Recv_Count))));
         else
            Recv_Count := Recv_Count + 1;
            Copy_Block.Append
              (Syn.Statements.New_Assignment_Statement
                 (Base_Name,
                  Syn.Expressions.New_Function_Call_Expression
                    ("Get_Data_Word",
                     Syn.Object ("Params"),
                     Syn.Literal (Recv_Count))));
         end if;
      end Copy_Scalar_Type;

   begin
      for Arg of Args loop
         declare
            Mode       : constant IDL_Argument_Mode := Get_Mode (Arg);
            Arg_Type   : constant IDL_Type := Get_Type (Arg);
         begin
            if Mode /= In_Argument then
               Copy_Scalar_Type (Block, Get_Ada_Name (Arg), Arg_Type);
            end if;
         end;
      end loop;

      for Arg of Args loop
         declare
            Mode       : constant IDL_Argument_Mode := Get_Mode (Arg);
            Arg_Type   : constant IDL_Type := Get_Type (Arg);
         begin
            if Mode /= In_Argument
              and then not Is_Scalar (Arg_Type)
            then
               Copy_Array_Type
                 (Block, Get_Ada_Name (Arg),
                  (if Has_Last_Index_Argument (Arg)
                   then Get_Ada_Name (Get_Last_Index_Argument (Arg))
                   else ""),
                  Arg_Type);
            end if;
         end;
      end loop;

      if Is_Function (Subpr) then
         declare
            Ret      : Syn.Blocks.Block_Type;
            Ret_Type : constant IDL.Types.IDL_Type :=
                         Get_Return_Type (Subpr);
         begin
            if Is_String (Ret_Type) then
               Recv_Count := Recv_Count + 1;
               Ret.Add_Declaration
                 (Syn.Declarations.New_Constant_Declaration
                    ("Result",
                     "String",
                     Syn.Expressions.New_Function_Call_Expression
                       ("Rose.System_Calls.Marshalling.Copy_Memory_To_String",
                        Syn.Object ("Item." & Get_Ada_Name (Subpr)
                          & "_Result_Cap"),
                        Syn.Expressions.New_Function_Call_Expression
                          ("Get_Data_Word",
                           Syn.Object ("Params"),
                           Syn.Literal (Recv_Count)))));
            else
               Ret.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    ("Result", Get_Ada_Name (Get_Return_Type (Subpr))));

               Copy_Scalar_Type (Ret, "Result", Ret_Type);
            end if;

            Ret.Append
              (Syn.Statements.New_Return_Statement
                 (Syn.Object ("Result")));
            Block.Append
              (Syn.Statements.Declare_Statement (Ret));
         end;
      end if;
   end Copy_Invocation_Result;

   ----------------------------
   -- Copy_Server_Invocation --
   ----------------------------

   procedure Copy_Server_Invocation
     (Block       : in out Syn.Blocks.Block_Type'Class;
      Server_Type : String;
      Subpr       : IDL.Syntax.IDL_Subprogram)
   is
      use IDL.Syntax, IDL.Types;
      Args       : constant IDL_Argument_Array := Get_Arguments (Subpr);
      Recv_Words : Natural := 0;
      Repl_Words : Natural := 0;

      procedure Declare_Local
        (Argument       : IDL_Argument;
         Index_Argument : IDL_Argument;
         Base_Name      : String;
         Base_Type      : IDL_Type;
         Is_Constant    : Boolean);

      procedure Save_Result
        (Arg         : IDL_Argument;
         Base_Name   : String;
         Base_Type   : IDL_Type);

      -------------------
      -- Declare_Local --
      -------------------

      procedure Declare_Local
        (Argument       : IDL_Argument;
         Index_Argument : IDL_Argument;
         Base_Name      : String;
         Base_Type      : IDL_Type;
         Is_Constant    : Boolean)
      is
      begin
         if Is_Record_Type (Base_Type) then
            if not Is_Constant then
               for F in 1 .. Get_Field_Count (Base_Type) loop
                  Recv_Words := Recv_Words + 1;
                  Block.Add_Declaration
                    (Syn.Declarations.New_Constant_Declaration
                       (Get_Field_Name (Base_Type, F),
                        Get_Ada_Name (Get_Field_Type (Base_Type, F)),
                        Syn.Expressions.New_Function_Call_Expression
                          ("Get_Data_Word",
                           Syn.Object ("Parameters"),
                           Syn.Literal (Recv_Words))));
               end loop;
            end if;
         elsif Is_Scalar (Base_Type) then
            Recv_Words := Recv_Words + 1;
            if Is_Constant then
               Block.Add_Declaration
                 (Syn.Declarations.New_Constant_Declaration
                    (Base_Name,
                     Get_Ada_Name (Base_Type),
                     Syn.Expressions.New_Function_Call_Expression
                       ("Get_Data_Word",
                        Syn.Object ("Parameters"),
                        Syn.Literal (Recv_Words))));
            else
               Block.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    (Base_Name,
                     Get_Ada_Name (Base_Type),
                     Syn.Expressions.New_Function_Call_Expression
                       ("Get_Data_Word",
                        Syn.Object ("Parameters"),
                        Syn.Literal (Recv_Words))));
            end if;
         else
            Recv_Words := Recv_Words + 1;
            Block.Add_Declaration
              (Syn.Declarations.New_Constant_Declaration
                 (Base_Name & "_Cap",
                  "Rose.Capabilities.Capability",
                  Syn.Expressions.New_Function_Call_Expression
                    ("Get_Data_Word",
                     Syn.Object ("Parameters"),
                     Syn.Literal (Recv_Words))));

            if Is_String (Base_Type) then
               if Get_Mode (Argument) /= Out_Argument then
                  Recv_Words := Recv_Words + 1;
                  Block.Add_Declaration
                    (Syn.Declarations.New_Constant_Declaration
                       (Get_Ada_Name (Argument) & "_Size",
                        "System.Storage_Elements.Storage_Count",
                        Syn.Expressions.New_Function_Call_Expression
                          ("Get_Data_Word",
                           Syn.Object ("Parameters"),
                           Syn.Literal (Recv_Words))));
               end if;
            elsif Has_Last_Index_Argument (Argument) then
               Recv_Words := Recv_Words + 1;
               Block.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    (Get_Ada_Name (Index_Argument),
                     Get_Ada_Name (Get_Type (Index_Argument)),
                     Syn.Expressions.New_Function_Call_Expression
                       ("Get_Data_Word",
                        Syn.Object ("Parameters"),
                        Syn.Literal (Recv_Words))));
               Block.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    (Base_Name,
                     Syn.Constrained_Subtype
                       (Get_Ada_Name (Get_Type (Argument)),
                        Syn.Literal (1),
                        Syn.Object (Get_Ada_Name (Index_Argument)))));
            else
               Block.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    (Base_Name,
                     Syn.Constrained_Subtype
                       ("System.Storage_Elements.Storage_Array",
                        Syn.Literal (1),
                        Syn.Object
                          (IDL.Syntax.Get_Length_Constraint
                               (Argument, "Server")))));
            end if;

            if Get_Mode (Argument) /= Out_Argument
              and then not Is_String (Get_Type (Argument))
            then

               Block.Add_Statement
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Copy_From_Memory_Cap",
                     Syn.Object (Base_Name & "_Cap"),
                     Syn.Object (Base_Name)));
            end if;
         end if;
      end Declare_Local;

      -----------------
      -- Save_Result --
      -----------------

      procedure Save_Result
        (Arg         : IDL_Argument;
         Base_Name   : String;
         Base_Type   : IDL_Type)
      is
         pragma Unreferenced (Arg);
      begin
         if Is_Record_Type (Base_Type) then
            for F in 1 .. Get_Field_Count (Base_Type) loop
               Repl_Words := Repl_Words + 1;
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Set_Data_Word",
                     Syn.Object ("Parameters"),
                     Syn.Literal (Repl_Words),
                     Syn.Object
                       (Base_Name & "."
                        & IDL.Identifiers.To_Ada_Name
                          (Get_Field_Name (Base_Type, F)))));
            end loop;
         elsif Is_Enumerated_Type (Base_Type) then
            Repl_Words := Repl_Words + 1;
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Set_Data_Word",
                  Syn.Object ("Parameters"),
                  Syn.Literal (Repl_Words),
                  Syn.Object
                    ("Rose.Words.Word'(" & Get_Ada_Name (Base_Type)
                     & "'Pos (" & Base_Name & "))")));
         elsif Is_Capability (Base_Type) then
            Repl_Words := Repl_Words + 1;
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Set_Data_Word",
                  Syn.Object ("Parameters"),
                  Syn.Literal (Repl_Words),
                  Syn.Object (Base_Name)));
         elsif Is_Interface (Base_Type) then
            Repl_Words := Repl_Words + 1;
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Set_Data_Word",
                  Syn.Object ("Parameters"),
                  Syn.Literal (Repl_Words),
                  Syn.Object ("Result.Interface_Capability")));
         elsif Is_String (Base_Type) then
            Repl_Words := Repl_Words + 1;
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Set_Data_Word",
                  Syn.Object ("Parameters"),
                  Syn.Literal (Repl_Words),
                  Syn.Expressions.New_Function_Call_Expression
                    ("Rose.Words.Word", "Result'Length")));
--         elsif Has_Last_Index_Argument (Arg) then

         elsif not Is_Scalar (Base_Type) then
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Copy_To_Memory_Cap",
                  Syn.Object (Base_Name & "_Cap"),
                  Syn.Object (Base_Name)));
         else
            Repl_Words := Repl_Words + 1;
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Set_Data_Word",
                  Syn.Object ("Parameters"),
                  Syn.Literal (Repl_Words),
                  Syn.Object (Base_Name)));
         end if;
      end Save_Result;

      Skipping : Boolean := False;
      Skip     : IDL_Argument;

   begin

      Block.Append
        (Syn.Statements.New_Procedure_Call_Statement
           ("Initialise_Reply",
            Syn.Object ("Parameters"),
            Syn.Literal (Repl_Words)));

      Block.Add_Declaration
        (Syn.Declarations.New_Constant_Declaration
           (Name        => "Server",
            Object_Type => Server_Type,
            Value       =>
              Syn.Expressions.New_Function_Call_Expression
                (Server_Type, "Server_Interface")));

      for Arg of Args loop
         if Skipping and then Arg = Skip then
            null;
         elsif Has_Last_Index_Argument (Arg) then
            Skip := Get_Last_Index_Argument (Arg);
            Skipping := True;
            Declare_Local
              (Arg, Skip, Get_Ada_Name (Arg), Get_Type (Arg),
               Get_Mode (Arg) = In_Argument);
         else
            Declare_Local
              (Arg, Arg, Get_Ada_Name (Arg), Get_Type (Arg),
               Get_Mode (Arg) = In_Argument);
         end if;

         if Is_String (Get_Type (Arg)) then
            Block.Add_Declaration
              (Syn.Declarations.New_Constant_Declaration
                 (Get_Ada_Name (Arg),
                  "String",
                  Syn.Expressions.New_Function_Call_Expression
                    ("Rose.System_Calls.Marshalling.Copy_Memory_To_String",
                     Syn.Object (Get_Ada_Name (Arg) & "_Cap"),
                     Syn.Object (Get_Ada_Name (Arg) & "_Size"))));
         end if;
      end loop;

      if Is_Function (Subpr) then
         declare
            Ret  : constant IDL.Types.IDL_Type := Get_Return_Type (Subpr);
            Expr : Syn.Expressions.Function_Call_Expression'Class :=
                     Syn.Expressions.New_Function_Call_Expression
                       ("Server." & Get_Ada_Name (Subpr));
         begin
            if not Is_Scalar (Ret) then
               Declare_Local (Get_Return_Argument (Subpr),
                              Get_Return_Argument (Subpr),
                              "Result", Ret, True);
            end if;

            for Arg of Args loop
               Expr.Add_Actual_Argument
                 (Syn.Object (Get_Ada_Name (Arg)));
            end loop;

            Block.Add_Declaration
              (Syn.Declarations.New_Constant_Declaration
                 ("Result",
                  IDL.Types.Get_Ada_Name (Ret),
                  Expr));
         end;
      else
         declare
            Call : Syn.Statements.Procedure_Call_Statement'Class :=
                     Syn.Statements.New_Procedure_Call_Statement
                       ("Server." & Get_Ada_Name (Subpr));
         begin
            for Arg of Args loop
               Call.Add_Actual_Argument (Get_Ada_Name (Arg));
            end loop;
            Block.Add_Statement (Call);
         end;
      end if;

      for Arg of Args loop
         if Get_Mode (Arg) /= In_Argument then
            Save_Result
              (Arg, Get_Ada_Name (Arg), Get_Type (Arg));
         end if;
      end loop;

      if Is_Function (Subpr) then
         declare
            Ret_Type : constant IDL.Types.IDL_Type :=
                         Get_Return_Type (Subpr);
         begin
            if Is_Record_Type (Ret_Type) then
               for F in 1 .. Get_Field_Count (Ret_Type) loop
                  Repl_Words := Repl_Words + 1;
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Set_Data_Word",
                        Syn.Object ("Parameters"),
                        Syn.Literal (Repl_Words),
                        Syn.Object
                          ("Result."
                           & IDL.Identifiers.To_Ada_Name
                             (Get_Field_Name (Ret_Type, F)))));
               end loop;
            elsif Is_Enumerated_Type (Ret_Type) then
               Repl_Words := Repl_Words + 1;
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Set_Data_Word",
                     Syn.Object ("Parameters"),
                     Syn.Literal (Repl_Words),
                     Syn.Object
                       ("Rose.Words.Word'(" & Get_Ada_Name (Ret_Type)
                        & "'Pos (Result))")));
            elsif Is_Capability (Ret_Type) then
               Repl_Words := Repl_Words + 1;
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Set_Data_Word",
                     Syn.Object ("Parameters"),
                     Syn.Literal (Repl_Words),
                     Syn.Object ("Result")));
            elsif Is_Interface (Ret_Type) then
               Repl_Words := Repl_Words + 1;
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Set_Data_Word",
                     Syn.Object ("Parameters"),
                     Syn.Literal (Repl_Words),
                     Syn.Object ("Result.Interface_Capability")));
            elsif Is_String (Ret_Type) then
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Rose.System_Calls.Marshalling.Copy_String_To_Memory",
                     Syn.Object ("Result_Cap"),
                     Syn.Object ("Result")));
               Repl_Words := Repl_Words + 1;
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Set_Data_Word",
                     Syn.Object ("Parameters"),
                     Syn.Literal (Repl_Words),
                     Syn.Expressions.New_Function_Call_Expression
                       ("Rose.Words.Word", "Result'Length")));
            else
               Repl_Words := Repl_Words + 1;
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Set_Data_Word",
                     Syn.Object ("Parameters"),
                     Syn.Literal (Repl_Words),
                     Syn.Object ("Result")));
            end if;
         end;
      end if;

      if Repl_Words > 0 then
         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Set_Reply_Words",
               Syn.Object ("Parameters"),
               Syn.Literal (Repl_Words)));
      end if;

      Block.Append
        (Syn.Statements.New_Procedure_Call_Statement
           ("Invoke_Capability",
            Syn.Object ("Parameters")));
   end Copy_Server_Invocation;

   ------------------------------
   -- Generate_Client_Override --
   ------------------------------

   procedure Generate_Client_Override
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Index   : Positive;
      Parent  : IDL.Syntax.IDL_Interface)
   is
      pragma Unreferenced (Parent);
      use IDL.Syntax, IDL.Identifiers;
      Name           : constant String := To_Ada_Name (Get_Name (Subpr));
      Interface_Name : constant String := Get_Ada_Name (Item);
      Block          : Syn.Blocks.Block_Type;
   begin

      Block.Add_Declaration
        (Syn.Declarations.Use_Package ("Rose.Invocation"));
      Block.Add_Declaration
        (Syn.Declarations.New_Object_Declaration
           (Identifiers => Syn.Declarations.Identifier ("Params"),
            Is_Aliased  => True,
            Is_Constant => False,
            Is_Deferred => False,
            Object_Type =>
              Syn.Named_Subtype
                ("Rose.System_Calls.Invocation_Record")));

      Initialise_Invocation (Block, Subpr, Index);

      Block.Add_Statement
        (Syn.Statements.New_Procedure_Call_Statement
           ("Invoke_Capability",
            Syn.Object ("Params")));

      Copy_Invocation_Result (Block, Subpr);

      declare
         use Syn.Declarations;
         Method : Subprogram_Declaration'Class :=
                    (if not Is_Function (Subpr)
                     then New_Procedure (Name, Block)
                     else New_Function
                       (Name,
                        IDL.Types.Get_Ada_Name
                          (Get_Return_Type (Subpr)),
                        Block));
      begin
         Method.Set_Overriding;
         Method.Add_Formal_Argument
           ("Item",
            (if Is_Function (Subpr) then In_Argument else Inout_Argument),
            Interface_Name & "_Client_Interface");
         declare
            Args : constant IDL_Argument_Array :=
                     Get_Arguments (Subpr);
         begin
            for Arg of Args loop
               Method.Add_Formal_Argument
                 (To_Ada_Name (Get_Name (Arg)),
                  Get_Syn_Mode (Arg),
                  IDL.Types.Get_Ada_Name
                    (Get_Type (Arg)));
            end loop;
         end;

         Method.Set_Private_Spec;
         Pkg.Append (Method);
      end;
   end Generate_Client_Override;

   -----------------------------
   -- Generate_Client_Package --
   -----------------------------

   procedure Generate_Client_Package (Item : IDL.Syntax.IDL_Interface) is
      use IDL.Syntax, IDL.Identifiers;
      Interface_Name : constant String :=
                         To_Ada_Name (Get_Name (Item));
      Package_Name   : constant String :=
                         Interface_Name & ".Client";
      Subprs         : constant IDL_Subprogram_Array :=
                         Get_Subprograms (Item);
      Client         : Syn.Declarations.Package_Type :=
                         Syn.Declarations.New_Package_Type
                           ("Rose.Interfaces." & Package_Name);
   begin

      --        Add_Context (Item, "Rose.Capabilities");

--        for I in 1 .. Get_Num_Contexts (Item) loop
--           Client.With_Package (Get_Context (Item, I));
--        end loop;

      Client.With_Package ("Rose.Invocation", Body_With => True);
      Client.With_Package ("Rose.System_Calls", Body_With => True,
                           Use_Package  => True);
      --  Client.With_Package ("Rose.Words", Body_With => True);

      With_Packages (Item, Client, False);

      declare
         Block : Syn.Blocks.Block_Type;
         Alloc : Syn.Expressions.Allocation_Expression :=
                   Syn.Expressions.New_Allocation_Expression
                     (Interface_Name & "_Client_Interface");

         procedure Initialise_Cap
           (Subpr : IDL_Subprogram);

         procedure Initialise_Constraint
           (Name            : String;
            Constraint_Type : IDL.Types.IDL_Type);

         procedure Set_Constraint_Value
           (Name            : String;
            Constraint_Type : IDL.Types.IDL_Type);

         --------------------
         -- Initialise_Cap --
         --------------------

         procedure Initialise_Cap
           (Subpr : IDL_Subprogram)
         is
            Args : constant IDL_Argument_Array :=
                     Get_Arguments (Subpr);
         begin
            for Arg of Args loop
               if not IDL.Types.Is_Scalar (Get_Type (Arg)) then
                  Alloc.Add_Initialiser
                    (Get_Name (Subpr)
                     & "_"
                     & Get_Ada_Name (Arg)
                     & "_Cap",
                     "Create_Memory_Cap");
               end if;
            end loop;

            if Is_Function (Subpr)
              and then IDL.Types.Is_String (Get_Return_Type (Subpr))
            then
               Alloc.Add_Initialiser
                 (Get_Name (Subpr) & "_Result_Cap",
                  "Create_Memory_Cap");
            end if;
         end Initialise_Cap;

         ---------------------------
         -- Initialise_Constraint --
         ---------------------------

         procedure Initialise_Constraint
           (Name            : String;
            Constraint_Type : IDL.Types.IDL_Type)
         is
            pragma Unreferenced (Constraint_Type);
         begin
            Alloc.Add_Initialiser ("Cached_" & Name, Syn.Literal (0));
         end Initialise_Constraint;

         --------------------------
         -- Set_Constraint_Value --
         --------------------------

         procedure Set_Constraint_Value
           (Name            : String;
            Constraint_Type : IDL.Types.IDL_Type)
         is
            pragma Unreferenced (Constraint_Type);
         begin
            Block.Append
              (Syn.Statements.New_Assignment_Statement
                 (Target => "Client.Cached_" & Name,
                  Value  => Syn.Object ("Client." & Name)));
         end Set_Constraint_Value;

      begin

         Alloc.Add_Initialiser
           ("Interface_Cap", "Client_Cap");

         Scan_Constraints (Item, True, Initialise_Constraint'Access);
         Scan_Subprograms (Item, True, Initialise_Cap'Access);

         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              (Name        => "Client",
               Object_Type => Interface_Name & "_Client_Access",
               Value       => Alloc));

         Scan_Constraints (Item, True, Set_Constraint_Value'Access);

         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   (Interface_Name & "_Interface",
                    Syn.Object ("Client"))));

         declare
            Create_Client : Syn.Declarations.Subprogram_Declaration'Class :=
                              Syn.Declarations.New_Function
                                ("Create_Client",
                                 Interface_Name & "_Interface",
                                 Block);
         begin
            Create_Client.Add_Formal_Argument
              ("Client_Cap", "Rose.Capabilities.Capability");
            Client.Append (Create_Client);
         end;
      end;

      Generate_Interface_Record
        (Pkg    => Client,
         Item   => Item,
         Client => True);

      for I in 1 .. Get_Num_Inherited (Item) loop
         Generate_Inherited_Overrides (Client, Item,
                                       Get_Inherited (Item, I));
      end loop;

      for I in Subprs'Range loop
         Generate_Client_Override (Client, Item, Subprs (I), I,
                                   Null_Interface);
      end loop;

      declare
         Writer : Syn.File_Writer.File_Writer;
      begin
         Client.Write (Writer);
      end;

   end Generate_Client_Package;

   ----------------------------------
   -- Generate_Inherited_Overrides --
   ----------------------------------

   procedure Generate_Inherited_Overrides
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Parent  : IDL.Syntax.IDL_Interface)
   is
      use IDL.Syntax;
      Subprs  : constant IDL_Subprogram_Array :=
                  Get_Subprograms (Parent);
   begin
      for I in 1 .. Get_Num_Inherited (Parent) loop
         Generate_Inherited_Overrides (Pkg, Item,
                                       Get_Inherited (Parent, I));
      end loop;

      for I in Subprs'Range loop
         Generate_Client_Override (Pkg, Item, Subprs (I), I, Parent);
      end loop;
   end Generate_Inherited_Overrides;

   ------------------------
   -- Generate_Interface --
   ------------------------

   procedure Generate_Interface (Item : IDL.Syntax.IDL_Interface) is
   begin
      Generate_Interface_Package (Item);
      Generate_Client_Package (Item);
      Generate_Server_Package (Item);
   end Generate_Interface;

   --------------------------------
   -- Generate_Interface_Package --
   --------------------------------

   procedure Generate_Interface_Package (Item : IDL.Syntax.IDL_Interface) is
      use IDL.Syntax, IDL.Identifiers;
      Interface_Name : constant String :=
                         To_Ada_Name (Get_Name (Item));
      Package_Name   : constant String :=
                         Interface_Name;
      Type_Name      : constant String :=
                         "Root_" & Interface_Name & "_Interface";
      Access_Name    : constant String :=
                         Interface_Name & "_Interface";
      Subprs         : constant IDL_Subprogram_Array :=
                         Get_Subprograms (Item);
      IF_Pkg         : Syn.Declarations.Package_Type :=
                         Syn.Declarations.New_Package_Type
                           ("Rose.Interfaces." & Package_Name);
   begin

      for I in 1 .. Get_Num_Contexts (Item) loop
         IF_Pkg.With_Package (Get_Context (Item, I));
      end loop;

      for I in 1 .. Get_Num_Objects (Item) loop

         if Get_Object_Is_Constant (Item, I) then
            declare
               use type IDL.Types.IDL_Type;
               Object_Type : constant IDL.Types.IDL_Type :=
                               Get_Object_Type (Item, I);
            begin
               if Object_Type = IDL.Types.No_Type then
                  IF_Pkg.Append
                    (Syn.Declarations.New_Constant_Declaration
                       (Get_Object_Ada_Name (Item, I),
                        Syn.Object
                          (Get_Object_Value (Item, I))));
               else
                  IF_Pkg.Append
                    (Syn.Declarations.New_Constant_Declaration
                       (Name        => Get_Object_Ada_Name (Item, I),
                        Object_Type =>
                          IDL.Types.Get_Ada_Name
                            (Get_Object_Type (Item, I)),
                        Value       =>
                          Syn.Object (Get_Object_Value (Item, I))));
               end if;
            end;
         elsif Get_Object_Is_Type (Item, I) then
            declare
               use IDL.Types;
               New_Type : constant IDL_Type :=
                            Get_Object_Type (Item, I);
            begin
               if Is_Record_Type (New_Type) then
                  declare
                     Rec : Syn.Types.Record_Type_Definition;
                  begin
                     for Field_Index in 1 .. Get_Field_Count (New_Type) loop
                        Rec.Add_Component
                          (To_Ada_Name
                             (Get_Field_Name
                                  (New_Type,
                                   Field_Index)),
                           Get_Ada_Name
                             (Get_Field_Type (New_Type, Field_Index)));
                     end loop;

                     IF_Pkg.Append
                       (Syn.Declarations.New_Full_Type_Declaration
                          (Get_Object_Ada_Name (Item, I),
                           Rec));
                  end;
               elsif Is_Enumerated_Type (New_Type) then
                  declare
                     Enum : Syn.Enumeration_Type_Definition;
                  begin
                     for Literal_Index in
                       1 .. Get_Literal_Count (New_Type)
                     loop
                        Enum.New_Literal
                          (Get_Literal_Name (New_Type, Literal_Index));
                     end loop;

                     IF_Pkg.Append
                       (Syn.Declarations.New_Full_Type_Declaration
                          (Get_Object_Ada_Name (Item, I),
                           Enum));
                  end;
               else
                  IF_Pkg.Append
                    (Syn.Declarations.New_Full_Type_Declaration
                       (Get_Object_Ada_Name (Item, I),
                        Syn.New_Derived_Type
                          (IDL.Types.Get_Ada_Name (New_Type))));
               end if;
            end;
         end if;
      end loop;

      declare
         IF_Type : Syn.Interface_Type_Definition;
      begin
         IF_Type.Set_Limited;
         for I in 1 .. Get_Num_Inherited (Item) loop
            IF_Type.Add_Parent
              (Get_Qualified_Root_Name (Get_Inherited (Item, I)));
         end loop;
         IF_Type.Add_Parent ("Root_Rose_Interface");

         IF_Pkg.Append
           (Syn.Declarations.New_Full_Type_Declaration
              (Type_Name, IF_Type));
      end;

      IF_Pkg.Append
        (Syn.Declarations.New_Full_Type_Declaration
           (Access_Name,
            Syn.New_Access_Type
              (Type_Name & "'Class",
               Access_All => True)));

      for Subpr of Subprs loop
         declare
            use Syn.Declarations;
            Name   : constant String := Get_Name (Subpr);
            Method : Subprogram_Declaration :=
                       (if Is_Function (Subpr)
                        then New_Abstract_Function
                          (Name,
                           Syn.Named_Subtype
                             (IDL.Types.Get_Ada_Name
                                (Get_Return_Type (Subpr))))
                        else New_Abstract_Procedure (Name));
            Args   : constant IDL_Argument_Array :=
                       Get_Arguments (Subpr);

         begin
            Method.Add_Formal_Argument
              (Arg_Name => "Item",
               Arg_Mode =>
                 (if Is_Function (Subpr)
                  then In_Argument else Inout_Argument),
               Arg_Type => Type_Name);

            for Arg of Args loop
               Method.Add_Formal_Argument
                 (Get_Name (Arg), Get_Syn_Mode (Arg),
                  IDL.Types.Get_Ada_Name (Get_Type (Arg)));
            end loop;

            IF_Pkg.Append (Method);
         end;

      end loop;

      declare
         Writer : Syn.File_Writer.File_Writer;
      begin
         IF_Pkg.Write (Writer);
      end;

   end Generate_Interface_Package;

   -------------------------------
   -- Generate_Interface_Record --
   -------------------------------

   procedure Generate_Interface_Record
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Client  : Boolean)
   is
      use IDL.Syntax, IDL.Identifiers;
      Interface_Name   : constant String :=
                           To_Ada_Name (Get_Name (Item));
      Record_Name      : constant String :=
                           Interface_Name & "_"
                           & (if Client then "Client" else "Server")
                           & "_Interface";
      Access_Name      : constant String :=
                           Interface_Name & "_"
                           & (if Client then "Client" else "Server")
                           & "_Access";
      Interface_Record : Syn.Types.Record_Type_Definition;
   begin
      Interface_Record.Add_Parent
        ("Root_" & Interface_Name & "_Interface");

      if not Client then
         Interface_Record.Add_Component
           ("Context", "Rose.Server.Server_Context");
      end if;

      Interface_Record.Add_Component
        ("Interface_Cap", "Rose.Capabilities.Capability");

      if Client then
         declare

            procedure Add_Buffer_Caps (Subpr : IDL_Subprogram);
            procedure Add_Constraint_Cache
              (Name : String;
               Constraint_Type : IDL.Types.IDL_Type);

            ---------------------
            -- Add_Buffer_Caps --
            ---------------------

            procedure Add_Buffer_Caps (Subpr : IDL_Subprogram) is
               Args : constant IDL_Argument_Array :=
                        Get_Arguments (Subpr);
            begin
               for Arg of Args loop
                  if not IDL.Types.Is_Scalar (Get_Type (Arg)) then
                     Interface_Record.Add_Component
                       (Get_Name (Subpr)
                        & "_"
                        & Get_Ada_Name (Arg)
                        & "_Cap",
                        "Rose.Capabilities.Capability");
                  end if;
               end loop;
               if Is_Function (Subpr)
                 and then IDL.Types.Is_String (Get_Return_Type (Subpr))
               then
                  Interface_Record.Add_Component
                    (Get_Name (Subpr) & "_Result_Cap",
                     "Rose.Capabilities.Capability");
               end if;
            end Add_Buffer_Caps;

            --------------------------
            -- Add_Constraint_Cache --
            --------------------------

            procedure Add_Constraint_Cache
              (Name            : String;
               Constraint_Type : IDL.Types.IDL_Type)
            is
            begin
               Interface_Record.Add_Component
                 ("Cached_" & Name,
                  IDL.Types.Get_Ada_Name (Constraint_Type));
            end Add_Constraint_Cache;

         begin
            Scan_Constraints (Item, True, Add_Constraint_Cache'Access);
            Scan_Subprograms (Item, True, Add_Buffer_Caps'Access);
         end;
      end if;

      if not Client then
         Interface_Record.Set_Abstract;
      end if;

      declare
         Type_Dec : Syn.Declaration'Class :=
                      Syn.Declarations.New_Private_Type_Declaration
                        (Record_Name,
                         Interface_Record);
      begin
         Type_Dec.Set_Private_Spec;
         Pkg.Append (Type_Dec);
      end;

      if Client then
         declare
            Access_Dec : constant Syn.Declaration'Class :=
                           Syn.Declarations.New_Full_Type_Declaration
                             (Access_Name,
                              Syn.New_Access_Type
                                (Record_Name & "'Class",
                                 Access_All => True));
         begin
            Pkg.Append_To_Body (Access_Dec);
         end;
      end if;

      declare
         Interface_Cap : Syn.Declarations.Subprogram_Declaration'Class :=
                           Syn.Declarations.New_Function
                             (Name        => "Interface_Capability",
                              Result_Type => "Rose.Capabilities.Capability",
                              Result      =>
                                Syn.Object ("Item.Interface_Cap"));
      begin
         Interface_Cap.Add_Formal_Argument
           (Arg_Name       => "Item",
            Arg_Type       => Record_Name);
         Interface_Cap.Set_Overriding;
         Interface_Cap.Set_Private_Spec;
         Pkg.Append (Interface_Cap);
      end;

   end Generate_Interface_Record;

   -----------------------------
   -- Generate_Server_Package --
   -----------------------------

   procedure Generate_Server_Package (Item : IDL.Syntax.IDL_Interface) is
      use IDL.Syntax, IDL.Identifiers;
      Server_Pkg     : Syn.Declarations.Package_Type;
      Interface_Name : constant String :=
                         To_Ada_Name (Get_Name (Item));
      Server_Name    : constant String :=
                         Interface_Name & "_Server_Interface";
      Package_Name   : constant String :=
                         Interface_Name & ".Server";
   begin

      Server_Pkg :=
        Syn.Declarations.New_Package_Type
          ("Rose.Interfaces." & Package_Name);

      Server_Pkg.With_Package ("Rose.Capabilities");
      --        Server_Pkg.With_Package ("Rose.Objects");

--      Server_Pkg.With_Package ("System", Body_With => True);
      Server_Pkg.With_Package ("Rose.Capabilities.Create", Body_With => True);
      Server_Pkg.With_Package ("Rose.System_Calls",
                               Body_With => True, Use_Package => True);
      Server_Pkg.With_Package ("Rose.Server");

      --      Server_Pkg.With_Package ("Rose.Words", Body_With => True);

      for I in 1 .. Get_Num_Contexts (Item) loop
         Server_Pkg.With_Package
           (Get_Context (Item, I), Body_With => True);
      end loop;

      With_Packages (Item, Server_Pkg, True);

      Generate_Interface_Record
        (Pkg    => Server_Pkg,
         Item   => Item,
         Client => False);

      declare
         procedure Generate_Subpr (Subpr : IDL_Subprogram);

         --------------------
         -- Generate_Subpr --
         --------------------

         procedure Generate_Subpr (Subpr : IDL_Subprogram) is
            Block : Syn.Blocks.Block_Type;
         begin

            Copy_Server_Invocation (Block,
                                    Get_Ada_Name (Item) &
                                      "_Interface",
                                    Subpr);

            declare
               Proc : Syn.Declarations.Subprogram_Declaration'Class :=
                        Syn.Declarations.New_Procedure
                          ("Handle_" & Get_Ada_Name (Subpr),
                           Block);
            begin
               Proc.Add_Formal_Argument
                 ("Server_Interface", "Rose_Interface");
               Proc.Add_Formal_Argument
                 (Arg_Name       => "Parameters",
                  Null_Exclusion => False,
                  Is_Aliased     => True,
                  Arg_Mode       => Syn.Declarations.Inout_Argument,
                  Arg_Type       =>
                    "Rose.System_Calls.Invocation_Record");
               Server_Pkg.Append_To_Body (Proc);
            end;
         end Generate_Subpr;

      begin
         Scan_Subprograms (Item, True, Generate_Subpr'Access);
      end;

      --  Generate attach/create server

      for Generate_Attach in Boolean loop

         declare
            Block       : Syn.Blocks.Block_Type;

            Endpoint    : Natural := 0;

            procedure Register_Endpoint (Subpr : IDL_Subprogram);

            -----------------------
            -- Register_Endpoint --
            -----------------------

            procedure Register_Endpoint (Subpr : IDL_Subprogram) is
            begin
               Endpoint := Endpoint + 1;
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Rose.Server.Register_Handler",
                     Syn.Object ("Server.Context"),
                     Syn.Object ("Server.Interface_Cap"),
                     Syn.Literal (Endpoint),
                     Syn.Object ("Server"),
                     Syn.Object
                       ("Handle_" & Get_Ada_Name (Subpr) & "'Access")));
            end Register_Endpoint;

         begin

            if Generate_Attach then
               Block.Append
                 (Syn.Statements.New_Assignment_Statement
                    ("Server.Context",
                     Syn.Object ("Context")));
            end if;

            Block.Append
              (Syn.Statements.New_Assignment_Statement
                 ("Server.Interface_Cap",
                  Syn.Expressions.New_Function_Call_Expression
                    ("Rose.Capabilities.Create.Create_Interface_Cap",
                     Syn.Literal (Get_Identity (Item)), --  interface identity
                     Syn.Literal (1),    --  first endpoint
                     Syn.Literal (Full_Subprogram_Count (Item)),
                     Syn.Object (if Generate_Attach
                       then "False" else "Default_Interface"))));

            Scan_Subprograms (Item, True, Register_Endpoint'Access);

            declare
               Proc_Name : constant String :=
                             (if Generate_Attach
                              then "Attach_Server"
                              else "Create_Server");
               Proc      : Syn.Declarations.Subprogram_Declaration'Class :=
                             Syn.Declarations.New_Procedure
                               (Proc_Name, Block);
            begin
               Proc.Add_Formal_Argument
                 (Arg_Name       => "Server",
                  Null_Exclusion => True,
                  Is_Aliased     => False,
                  Arg_Mode       => Syn.Declarations.Access_Argument,
                  Arg_Type       => Server_Name & "'Class");

               if Generate_Attach then
                  Proc.Add_Formal_Argument
                    (Arg_Name       => "Context",
                     Arg_Type       => "Rose.Server.Server_Context");
               else
                  Proc.Add_Formal_Argument
                    (Arg_Name       => "Default_Interface",
                     Arg_Type       => "Boolean",
                     Arg_Default    => Syn.Object ("True"));
               end if;

               Server_Pkg.Append (Proc);

            end;
         end;
      end loop;

      for Provide_Cap in Boolean loop
         declare
            Block : Syn.Blocks.Block_Type;
            Call  : Syn.Statements.Procedure_Call_Statement'Class :=
                      Syn.Statements.New_Procedure_Call_Statement
                        ("Rose.Server.Receive",
                         Syn.Object ("Server.Context"));
         begin
            if Provide_Cap then
               Call.Add_Actual_Argument ("Receive_Cap");
            end if;

            Block.Append
              (Syn.Statements.While_Statement
                 (Condition  => Syn.Object ("True"),
                  While_Body => Call));

            declare
               use Syn.Declarations;
               Start_Proc : Subprogram_Declaration'Class :=
                              New_Procedure
                                ("Start_Server",
                                 Block);
            begin
               Start_Proc.Add_Formal_Argument
                 (Arg_Name       => "Server",
                  Null_Exclusion => True,
                  Is_Aliased     => False,
                  Arg_Mode       => Syn.Declarations.Access_Argument,
                  Arg_Type       => Server_Name & "'Class");
               if Provide_Cap then
                  Start_Proc.Add_Formal_Argument
                    (Arg_Name => "Receive_Cap",
                     Arg_Type => "Rose.Capabilities.Capability");
               end if;
               Server_Pkg.Append (Start_Proc);
            end;
         end;
      end loop;

      declare
         Interface_Cap : Syn.Declarations.Subprogram_Declaration'Class :=
                           Syn.Declarations.New_Function
                             (Name        => "Interface_Cap",
                              Result_Type => "Rose.Capabilities.Capability",
                              Result      =>
                                Syn.Object ("Server.Interface_Cap"));
      begin
         Interface_Cap.Add_Formal_Argument
           (Arg_Name       => "Server",
            Arg_Mode       => Syn.Declarations.Access_Argument,
            Arg_Type       => Server_Name & "'Class");
         Server_Pkg.Append (Interface_Cap);
      end;

      declare
         Server_Context : Syn.Declarations.Subprogram_Declaration'Class :=
                           Syn.Declarations.New_Function
                             (Name        => "Server_Context",
                              Result_Type => "Rose.Server.Server_Context",
                              Result      =>
                                Syn.Object ("Server.Context"));
      begin
         Server_Context.Add_Formal_Argument
           (Arg_Name       => "Server",
            Arg_Type       => Server_Name & "'Class");
         Server_Pkg.Append (Server_Context);
      end;

      declare
         Writer : Syn.File_Writer.File_Writer;
      begin
         Server_Pkg.Write (Writer);
      end;

   end Generate_Server_Package;

   ---------------------------
   -- Initialise_Invocation --
   ---------------------------

   procedure Initialise_Invocation
     (Block : in out Syn.Blocks.Block_Type'Class;
      Subpr : IDL.Syntax.IDL_Subprogram;
      Index : Positive)
   is
      use IDL.Syntax, IDL.Types;
      Args       : constant IDL_Argument_Array := Get_Arguments (Subpr);
      Send_Words : Natural := 0;
      Recv_Words : Natural := 0;
   begin
      for I in Args'Range loop
         declare
            Mode     : constant IDL_Argument_Mode := Get_Mode (Args (I));
            Arg_Type : constant IDL_Type := Get_Type (Args (I));
            Cap_Name : constant String :=
                         "Item."
                         & Get_Name (Subpr)
                         & "_"
                         & Get_Ada_Name (Args (I))
                         & "_Cap";
         begin

            if Mode = In_Argument
              or else Mode = Inout_Argument
              or else not Is_Scalar (Arg_Type)
            then
               if Is_Capability (Arg_Type) then
                  Send_Words := Send_Words + 1;
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Set_Data_Word",
                        Syn.Object ("Params"),
                        Syn.Literal (Send_Words),
                        Syn.Object (Get_Ada_Name (Args (I)))));
               elsif not Is_Scalar (Arg_Type) then
                  if Mode = In_Argument
                    or else Mode = Inout_Argument
                  then
                     if Is_String (Arg_Type) then
                        Block.Append
                          (Syn.Statements.New_Procedure_Call_Statement
                             ("Rose.System_Calls.Marshalling."
                              & "Copy_String_To_Memory",
                              Syn.Object (Cap_Name),
                              Syn.Object (Get_Ada_Name (Args (I)))));
                     else
                        Block.Append
                          (Syn.Statements.New_Procedure_Call_Statement
                             ("Copy_To_Memory_Cap",
                              Syn.Object (Cap_Name),
                              Syn.Object (Get_Ada_Name (Args (I)))));
                     end if;
                  end if;
                  Send_Words := Send_Words + 1;
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Set_Data_Word",
                        Syn.Object ("Params"),
                        Syn.Literal (Send_Words),
                        Syn.Object
                          ("Item."
                           & Get_Name (Subpr)
                           & "_"
                           & Get_Ada_Name (Args (I))
                           & "_Cap")));
                  if Is_String (Arg_Type) then
                     Send_Words := Send_Words + 1;
                     Block.Append
                       (Syn.Statements.New_Procedure_Call_Statement
                          ("Set_Data_Word",
                           Syn.Object ("Params"),
                           Syn.Literal (Send_Words),
                           Syn.Expressions.New_Function_Call_Expression
                             ("Rose.Words.Word",
                              Syn.Object
                                (Get_Ada_Name (Args (I))
                                 & "'Length"))));
                  elsif Is_Stream (Arg_Type) then
                     Send_Words := Send_Words + 1;
                     Block.Append
                       (Syn.Statements.New_Procedure_Call_Statement
                          ("Set_Data_Word",
                           Syn.Object ("Params"),
                           Syn.Literal (Send_Words),
                           Syn.Object (Get_Ada_Name (Args (I)) & "'Last")));
                  elsif Has_Last_Index_Argument (Args (I)) then
                     Send_Words := Send_Words + 1;
                     Block.Append
                       (Syn.Statements.New_Procedure_Call_Statement
                          ("Set_Data_Word",
                           Syn.Object ("Params"),
                           Syn.Literal (Send_Words),
                           Syn.Object (Get_Ada_Name (Args (I)) & "'Last")));
                  end if;
               else
                  Send_Words := Send_Words + 1;
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Set_Data_Word",
                        Syn.Object ("Params"),
                        Syn.Literal (Send_Words),
                        Syn.Object
                          (Get_Ada_Name (Args (I))
                           & (if Is_Interface (Arg_Type)
                             then ".Get_Type"
                             else ""))));
               end if;
            end if;

            if Mode = Out_Argument or else Mode = Inout_Argument then
               Recv_Words := Recv_Words + 1;
            end if;
         end;
      end loop;

      if Is_Function (Subpr) then
         declare
            Ret_Type : constant IDL_Type := Get_Return_Type (Subpr);
         begin
            if Is_String (Ret_Type) then
               Send_Words := Send_Words + 1;
               Recv_Words := Recv_Words + 1;
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Set_Data_Word",
                     Syn.Object ("Params"),
                     Syn.Literal (Send_Words),
                     Syn.Object
                       ("Item."
                        & Get_Name (Subpr)
                        & "_Result_Cap")));
            else
               Recv_Words := Recv_Words + 1;
            end if;
         end;
      end if;

      declare
         Ctrl : Syn.Blocks.Block_Type;

         procedure Assign (Component, Value : String);

         procedure Assign (Component : String;
                           Value     : Syn.Expression'Class);

         ------------
         -- Assign --
         ------------

         procedure Assign (Component, Value : String) is
         begin
            Assign (Component, Syn.Object (Value));
         end Assign;

         ------------
         -- Assign --
         ------------

         procedure Assign (Component : String;
                           Value     : Syn.Expression'Class)
         is
         begin
            Ctrl.Append
              (Syn.Statements.New_Assignment_Statement
                 ("Control." & Component, Value));
         end Assign;

      begin
         Ctrl.Add_Declaration
           (Syn.Declarations.Renaming_Declaration
              ("Control", "Control_Word", Syn.Object ("Params.Control")));
         Assign ("Flags (Send)", "True");
         --  Always block, since it's using IPC semantics
         Assign ("Flags (Block)", "True");

         --  if we're sending caps, always send them single-use
         Assign ("Alloc_Count", Syn.Literal (1));

         Assign ("Sent_Word_Count", Syn.Literal (Send_Words));

         --  FIXME: always claim that we receive the maximum
         --  possible number of words if we receive any (!)

         if Recv_Words > 0 then
            Assign ("Recv_Word_Count", Syn.Literal (15));
         else
            Assign ("Recv_Word_Count", Syn.Literal (0));
         end if;

         Block.Append
           (Syn.Statements.Declare_Statement (Ctrl));
      end;

      Block.Append
        (Syn.Statements.New_Assignment_Statement
           ("Params.Cap",
            Syn.Object ("Item.Interface_Cap")));
      Block.Append
        (Syn.Statements.New_Assignment_Statement
           ("Params.Control.Endpoint",
            Syn.Literal (Index)));

   end Initialise_Invocation;

   -------------------
   -- With_Packages --
   -------------------

   procedure With_Packages
     (Top_Interface : IDL.Syntax.IDL_Interface;
      Pkg           : in out Syn.Declarations.Package_Type'Class;
      Server        : Boolean)
   is
      use IDL.Syntax;

      procedure Add_Dependent_Withs (Subpr : IDL_Subprogram);

      procedure Check_Type
        (Ref : IDL.Types.IDL_Type;
         Is_Return : Boolean);

      -------------------------
      -- Add_Dependent_Withs --
      -------------------------

      procedure Add_Dependent_Withs (Subpr : IDL_Subprogram) is
      begin
         for Arg of Get_Arguments (Subpr) loop
            Check_Type (Get_Type (Arg), Is_Return => False);
         end loop;
         if Is_Function (Subpr) then
            Check_Type (Get_Return_Type (Subpr), Is_Return => True);
         end if;
      end Add_Dependent_Withs;

      ----------------
      -- Check_Type --
      ----------------

      procedure Check_Type
        (Ref       : IDL.Types.IDL_Type;
         Is_Return : Boolean)
      is
         pragma Unreferenced (Is_Return);
      begin
         if IDL.Types.Is_Interface (Ref) then
--              if Server then
--                 Pkg.With_Package ("Rose.Invocation", Body_With => True);
--              end if;
            declare
               Pkg_Name : constant String :=
                            IDL.Types.Get_Ada_Package (Ref);
            begin
               if Server then
                  Pkg.With_Package (Pkg_Name, Body_With => True);
               else
                  Pkg.With_Package (Pkg_Name, Private_With => True);
                  Pkg.With_Package (Pkg_Name & ".Client", Body_With => True);
               end if;
            end;
         else
            declare
               Pkg_Name : constant String :=
                            IDL.Types.Get_Ada_Package (Ref);
            begin
               if Pkg_Name /= ""  then
                  Pkg.With_Package
                    (Pkg_Name,
                     Private_With => not Server,
                     Body_With    => Server);
               end if;
            end;
            if Server
              and then IDL.Types.Is_String (Ref)
            then
               Pkg.With_Package ("System.Storage_Elements",
                                 Body_With => True);
            end if;

            if IDL.Types.Is_String (Ref) then
               Pkg.With_Package ("Rose.System_Calls.Marshalling",
                                 Body_With => True);
            end if;

            if IDL.Types.Is_Stream (Ref) then
               Pkg.With_Package ("Rose.System_Calls.Streams",
                                 Body_With => True, Use_Package => True);
            end if;

         end if;
      end Check_Type;

   begin
      --        for I in 1 .. Get_Num_Inherited (Top_Interface) loop
      --           Pkg.With_Package
      --             ("Rose.Interfaces."
      --              & Get_Ada_Name (Get_Inherited (Top_Interface, I))
      --              & ".Server",
      --              Body_With => True);
      --        end loop;

      Scan_Subprograms (Top_Interface, True, Add_Dependent_Withs'Access);
   end With_Packages;

end IDL.Generate;
