with Ada.Text_IO;

with IDL.Endpoints;
with IDL.Identifiers;
with IDL.Interface_Table;
with IDL.Options;
with IDL.Types;

with Syn.Blocks;
with Syn.Declarations;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;
with Syn.File_Writer;

package body IDL.Generate_Kernel is

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

   procedure Generate_Open_Interface
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface);

   procedure Generate_Inherited_Overrides
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Parent  : IDL.Syntax.IDL_Interface);

   procedure Generate_Client_Override
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Parent  : IDL.Syntax.IDL_Interface);

   procedure Generate_Server_Override
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Parent  : IDL.Syntax.IDL_Interface);

   procedure Generate_Server_Subprogram_Type
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Parent  : IDL.Syntax.IDL_Interface);

   procedure Generate_Cap_Query
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Parent  : IDL.Syntax.IDL_Interface);

   procedure Generate_Interface_Record
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Client  : Boolean);

   procedure Initialise_Invocation
     (Block : in out Syn.Blocks.Block_Type'Class;
      Subpr : IDL.Syntax.IDL_Subprogram);

   procedure Copy_Invocation_Result
     (Block : in out Syn.Blocks.Block_Type'Class;
      Subpr : IDL.Syntax.IDL_Subprogram);

   procedure Copy_Server_Invocation
     (Block       : in out Syn.Blocks.Block_Type'Class;
      Subpr       : IDL.Syntax.IDL_Subprogram);

   procedure With_Packages
     (Top_Interface : IDL.Syntax.IDL_Interface;
      Pkg           : in out Syn.Declarations.Package_Type'Class;
      Server        : Boolean);

   function Default_Size return Natural
   is (if IDL.Options.Generate_32_Bit then 32 else 64);

   procedure Check_Error
     (Block : in out Syn.Blocks.Block_Type'Class);

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error
     (Block : in out Syn.Blocks.Block_Type'Class)
   is
   begin
      Block.Append
        (Syn.Statements.New_Assignment_Statement
           (Target => "Last_Error",
            Value  => Syn.Object ("Rose.Invocation.OK")));
      Block.Append
        (Syn.Statements.If_Statement
           (Syn.Expressions.New_Function_Call_Expression
                ("Params.Control.Flags",
                 Syn.Object ("Rose.Invocation.Error")),
            Syn.Statements.New_Assignment_Statement
              ("Last_Error",
               Syn.Object ("Params.Error"))));
   end Check_Error;

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
         pragma Unreferenced (Base_Type, Count_Name);
         Call : Syn.Statements.Procedure_Call_Statement'Class :=
                  Syn.Statements.New_Procedure_Call_Statement
                    ("Rose.System_Calls.Copy_Received_Buffer");
      begin
         Call.Add_Actual_Argument
           (Syn.Object (Base_Name & "'Length"));
         Call.Add_Actual_Argument
           (Syn.Object (Base_Name & "'Address"));

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
                       ("Params.Data",
                        Syn.Literal (Recv_Count - 1)))));
         elsif Is_Interface (Base_Type) then

            IDL.Interface_Table.Check (Get_Name (Base_Type));

            declare
               Result : constant IDL_Interface :=
                          IDL.Interface_Table.Element (Get_Name (Base_Type));
               Cap_Index : Natural := 0;
               Open_Proc : Syn.Statements.Procedure_Call_Statement'Class :=
                             Syn.Statements.New_Procedure_Call_Statement
                               (Get_Package_Name (Result)
                                & ".Client.Open_Cap_Set",
                                Syn.Object (Base_Name));

               procedure Copy_Cap (Result_Subprogram : IDL_Subprogram);

               --------------
               -- Copy_Cap --
               --------------

               procedure Copy_Cap (Result_Subprogram : IDL_Subprogram) is
               begin
                  Open_Proc.Add_Actual_Argument
                    (Get_Ada_Name (Result_Subprogram),
                     Syn.Expressions.New_Function_Call_Expression
                       ("Params.Caps", Syn.Literal (Cap_Index)));
                  Cap_Index := Cap_Index + 1;
               end Copy_Cap;

            begin

               Scan_Subprograms (Result, True, Copy_Cap'Access);
               Copy_Block.Append (Open_Proc);

            end;

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
            if Get_Size (Base_Type) > Default_Size then
               declare
                  Get : constant Syn.Expression'Class :=
                          Syn.Expressions.New_Function_Call_Expression
                            ("Rose.System_Calls.Get_Word_64",
                             Syn.Object ("Params"),
                             Syn.Literal (Recv_Count));
               begin
                  Copy_Block.Append
                    (Syn.Statements.New_Assignment_Statement
                       (Base_Name,
                        (if Is_Word_Type (Base_Type)
                         then Get
                         else Syn.Expressions.New_Function_Call_Expression
                           (Get_Ada_Name (Base_Type), Get))));
               end;
               Recv_Count := Recv_Count + 2;
            else
               declare
                  Get : constant Syn.Expression'Class :=
                          Syn.Expressions.New_Function_Call_Expression
                            ("Rose.System_Calls.Get_Word_32",
                             Syn.Object ("Params"),
                             Syn.Literal (Recv_Count));
               begin
                  Copy_Block.Append
                    (Syn.Statements.New_Assignment_Statement
                       (Base_Name,
                        (if Is_Word_Type (Base_Type) then Get
                         else Syn.Expressions.New_Function_Call_Expression
                           (Get_Ada_Name (Base_Type), Get))));
               end;
               Recv_Count := Recv_Count + 1;
            end if;
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
         if Has_Scalar_Result (Subpr) then
            declare
               Ret      : Syn.Blocks.Block_Type;
               Ret_Type : constant IDL.Types.IDL_Type :=
                            Get_Return_Type (Subpr);
            begin
               Ret.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    ("Result", Get_Ada_Name (Get_Return_Type (Subpr))));

               Copy_Scalar_Type (Ret, "Result", Ret_Type);

               Ret.Append
                 (Syn.Statements.New_Return_Statement
                    (Syn.Object ("Result")));
               Block.Append
                 (Syn.Statements.Declare_Statement (Ret));
            end;
         elsif Is_String (Get_Return_Type (Subpr)) then
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Rose.System_Calls.Copy_Text",
                  Syn.Object ("Params"),
                  Syn.Expressions.New_Function_Call_Expression
                    ("Natural", Syn.Object ("Params.Data (0)")),
                  Syn.Object ("Result"),
                  Syn.Object ("Last")));
         end if;
      end if;

   end Copy_Invocation_Result;

   ----------------------------
   -- Copy_Server_Invocation --
   ----------------------------

   procedure Copy_Server_Invocation
     (Block       : in out Syn.Blocks.Block_Type'Class;
      Subpr       : IDL.Syntax.IDL_Subprogram)
   is
      use IDL.Syntax, IDL.Types;
      Args       : constant IDL_Argument_Array := Get_Arguments (Subpr);
      Recv_Words : Natural := 0;
      Repl_Words : Natural := 0;
      Recv_Caps  : Natural := 0;
      --  Repl_Caps  : Natural := 0;

      procedure Declare_Local
        (Base_Name      : String;
         Base_Type      : IDL_Type;
         Initialize     : Boolean;
         Is_Constant    : Boolean);

      procedure Save_Result
        (Arg         : IDL_Argument;
         Base_Name   : String;
         Base_Type   : IDL_Type);

      -------------------
      -- Declare_Local --
      -------------------

      procedure Declare_Local
        (Base_Name      : String;
         Base_Type      : IDL_Type;
         Initialize     : Boolean;
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
         elsif Is_Interface (Base_Type) then
            Block.Add_Declaration
              (Syn.Declarations.New_Constant_Declaration
                 (Base_Name,
                  "Rose.Capabilities.Capability",
                  Syn.Expressions.New_Function_Call_Expression
                    ("Parameters.Caps",
                     Syn.Literal (Recv_Caps))));
            Recv_Caps := Recv_Caps + 1;
         elsif Is_Scalar (Base_Type) then
            if not Initialize then
               Block.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    (Base_Name,
                     Get_Ada_Name (Base_Type)));
            elsif Get_Size (Base_Type) > Default_Size then
               declare
                  Get : constant Syn.Expression'Class :=
                          Syn.Expressions.New_Function_Call_Expression
                            ("Rose.System_Calls.Get_Word_64",
                             Syn.Object ("Parameters"),
                             Syn.Literal (Recv_Words));
               begin
                  if Is_Constant then
                     Block.Add_Declaration
                       (Syn.Declarations.New_Constant_Declaration
                          (Base_Name,
                           Get_Ada_Name (Base_Type),
                           (if Is_Word_Type (Base_Type)
                            then Get
                            else Syn.Expressions.New_Function_Call_Expression
                              (Get_Ada_Name (Base_Type), Get))));
                  else
                     Block.Add_Declaration
                       (Syn.Declarations.New_Object_Declaration
                          (Base_Name,
                           Get_Ada_Name (Base_Type),
                           (if Is_Word_Type (Base_Type)
                            then Get
                            else Syn.Expressions.New_Function_Call_Expression
                              (Get_Ada_Name (Base_Type), Get))));
                  end if;
               end;
               Recv_Words := Recv_Words + 2;
            else
               declare
                  Get : constant Syn.Expression'Class :=
                          Syn.Expressions.New_Function_Call_Expression
                            ("Rose.System_Calls.Get_Word_32",
                             Syn.Object ("Parameters"),
                             Syn.Literal (Recv_Words));
               begin
                  if Is_Constant then
                     Block.Add_Declaration
                       (Syn.Declarations.New_Constant_Declaration
                          (Base_Name,
                           Get_Ada_Name (Base_Type),
                           (if Is_Word_Type (Base_Type) then Get
                            else Syn.Expressions.New_Function_Call_Expression
                              (Get_Ada_Name (Base_Type), Get))));
                  else
                     Block.Add_Declaration
                       (Syn.Declarations.New_Object_Declaration
                          (Base_Name,
                           Get_Ada_Name (Base_Type),
                           (if Is_Word_Type (Base_Type) then Get
                            else Syn.Expressions.New_Function_Call_Expression
                              (Get_Ada_Name (Base_Type), Get))));
                  end if;
               end;
               Recv_Words := Recv_Words + 1;
            end if;

         elsif Is_Capability_Array (Base_Type) then
            Block.Add_Declaration
              (Syn.Declarations.New_Object_Declaration
                 ("Caps",
                  Syn.Constrained_Subtype
                    ("Rose.Capabilities.Capability_Array",
                     Syn.Literal (1), Syn.Literal (16))));
            Block.Add_Declaration
              (Syn.Declarations.New_Object_Declaration
                 ("Cap_Count", Syn.Named_Subtype ("Natural")));
            Block.Add_Statement
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Rose.System_Calls.Copy_Received_Caps",
                  Syn.Object ("Parameters"),
                  Syn.Object ("Caps"),
                  Syn.Object ("Cap_Count")));
            declare
               use Syn, Syn.Expressions;
               Assign      : constant Syn.Statement'Class :=
                               Syn.Statements.New_Assignment_Statement
                                 ("Caps (I)",
                                  New_Function_Call_Expression
                                    ("Parameters.Caps",
                                     New_Function_Call_Expression
                                       ("Rose.Invocation.Capability_Index",
                                        Syn.Expressions.Operator
                                          ("-", Object ("I"), Literal (1)))));
               Seq         : Syn.Statements.Sequence_Of_Statements;
            begin
               Seq.Append (Assign);
               Block.Append
                 (Syn.Statements.For_Loop
                    ("I", Literal (1), Object ("Cap_Count"), False, Seq));
            end;

         else
            if Is_String (Base_Type) then
               Block.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    (Base_Name,
                     Syn.Constrained_Subtype
                       ("String",
                        Syn.Literal (1),
                        Syn.Expressions.New_Function_Call_Expression
                          ("Natural",
                           Syn.Object ("Parameters.Buffer_Length")))));
            else
               Block.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    (Base_Name,
                     Syn.Constrained_Subtype
                       ("System.Storage_Elements.Storage_Array",
                        Syn.Literal (1),
                        Syn.Object ("Parameters.Buffer_Length"))));
            end if;

            Block.Add_Declaration
              (Syn.Declarations.New_Pragma
                 ("Import", "Ada, " & Base_Name));
            Block.Add_Declaration
              (Syn.Declarations.Address_Representation_Clause
                 (Base_Name, Syn.Object ("Parameters.Buffer_Address")));

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
            null;
         else
            if Get_Size (Base_Type) > Default_Size then
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Rose.System_Calls.Send_Word",
                     Syn.Object ("Parameters"),
                     (if Is_Word_Type (Base_Type)
                      then Syn.Object (Base_Name)
                      else Syn.Expressions.New_Function_Call_Expression
                        ("Rose.Words.Word_64", Base_Name))));
            else
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Rose.System_Calls.Send_Word",
                     Syn.Object ("Parameters"),
                     (if Is_Word_Type (Base_Type)
                      then Syn.Object (Base_Name)
                      else Syn.Expressions.New_Function_Call_Expression
                        ("Rose.Words.Word", Base_Name))));
            end if;
         end if;
      end Save_Result;

      Skipping : Boolean := False;
      Skip     : IDL_Argument;

   begin

      for Arg of Args loop
         if Skipping and then Arg = Skip then
            null;
         elsif Has_Last_Index_Argument (Arg) then
            Skip := Get_Last_Index_Argument (Arg);
            Skipping := True;
            Declare_Local
              (Get_Ada_Name (Arg), Get_Type (Arg),
               Get_Mode (Arg) in In_Argument | Inout_Argument,
               Get_Mode (Arg) = In_Argument);
         else
            Declare_Local
              (Get_Ada_Name (Arg), Get_Type (Arg),
               Get_Mode (Arg) in In_Argument | Inout_Argument,
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

      Block.Append
        (Syn.Statements.New_Procedure_Call_Statement
           ("Rose.System_Calls.Initialize_Reply",
            Syn.Object ("Parameters"),
            Syn.Object ("Parameters.Reply_Cap")));

      if Is_Function (Subpr) then
         declare
            Ret  : constant IDL.Types.IDL_Type := Get_Return_Type (Subpr);
            Expr : Syn.Expressions.Function_Call_Expression'Class :=
                     Syn.Expressions.New_Function_Call_Expression
                       ("Local_" & Get_Ada_Name (Subpr));
         begin
            if not Is_Scalar (Ret) then
               Declare_Local ("Result", Ret, True, True);
            end if;

            Expr.Add_Actual_Argument
              (Syn.Object ("Parameters.Identifier"));
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
                       ("Local_" & Get_Ada_Name (Subpr));
         begin
            Call.Add_Actual_Argument
              ("Parameters.Identifier");
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
                    ("Send_Word",
                     Syn.Object ("Parameters"),
                     Syn.Object
                       ("Rose.Words.Word'(" & Get_Ada_Name (Ret_Type)
                        & "'Pos (Result))")));
            elsif Is_Capability (Ret_Type) then
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Send_Cap",
                     Syn.Object ("Parameters"),
                     Syn.Object ("Result")));
            elsif Is_Interface (Ret_Type) then
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Send_Cap",
                     Syn.Object ("Parameters"),
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
               if Get_Size (Ret_Type) > Default_Size then
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Rose.System_Calls.Send_Word",
                        Syn.Object ("Parameters"),
                        (if Is_Word_Type (Ret_Type)
                         then Syn.Object ("Result")
                         else Syn.Expressions.New_Function_Call_Expression
                           ("Rose.Words.Word_64", "Result"))));
               else
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Rose.System_Calls.Send_Word",
                        Syn.Object ("Parameters"),
                        (if Is_Word_Type (Ret_Type)
                         then Syn.Object ("Result")
                         else Syn.Expressions.New_Function_Call_Expression
                           ("Rose.Words.Word", "Result"))));
               end if;
            end if;
         end;
      end if;

   end Copy_Server_Invocation;

   ------------------------
   -- Generate_Cap_Query --
   ------------------------

   procedure Generate_Cap_Query
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Parent  : IDL.Syntax.IDL_Interface)
   is
      pragma Unreferenced (Parent);
      use IDL.Syntax, IDL.Identifiers;
      Name           : constant String := To_Ada_Name (Get_Name (Subpr));
      Interface_Name : constant String := Get_Ada_Name (Item);
      Get_Cap        : Syn.Declarations.Subprogram_Declaration'Class :=
                         Syn.Declarations.New_Function
                           ("Get_" & Name & "_Cap",
                            "Rose.Capabilities.Capability",
                            Syn.Object ("Item." & Name));
   begin
      Get_Cap.Add_Formal_Argument
        (Arg_Name => "Item",
         Arg_Type => Interface_Name & "_Client");
      Pkg.Add_Separator;
      Pkg.Append (Get_Cap);
   end Generate_Cap_Query;

   ------------------------------
   -- Generate_Client_Override --
   ------------------------------

   procedure Generate_Client_Override
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Parent  : IDL.Syntax.IDL_Interface)
   is
      pragma Unreferenced (Parent);
      use IDL.Syntax, IDL.Identifiers;
      Name           : constant String := To_Ada_Name (Get_Name (Subpr));
      Interface_Name : constant String := Get_Ada_Name (Item);
      Block          : Syn.Blocks.Block_Type;
   begin

      Block.Add_Declaration
        (Syn.Declarations.New_Object_Declaration
           (Identifiers => Syn.Declarations.Identifier ("Params"),
            Is_Aliased  => True,
            Is_Constant => False,
            Is_Deferred => False,
            Object_Type =>
              Syn.Named_Subtype
                ("Rose.Invocation.Invocation_Record")));

      Initialise_Invocation (Block, Subpr);

      Block.Add_Statement
        (Syn.Statements.New_Procedure_Call_Statement
           ("Rose.System_Calls.Invoke_Capability",
            Syn.Object ("Params")));

      Check_Error (Block);

      Copy_Invocation_Result (Block, Subpr);

      declare
         use Syn.Declarations;
         Method : Subprogram_Declaration'Class :=
                    (if not Is_Function (Subpr)
                       or else not Has_Scalar_Result (Subpr)
                     then New_Procedure (Name, Block)
                     else New_Function
                       (Name,
                        IDL.Types.Get_Ada_Name
                          (Get_Return_Type (Subpr)),
                        Block));
      begin
         Method.Add_Formal_Argument
           ("Item",
            In_Argument,
            Interface_Name & "_Client");
         declare
            use IDL.Types;
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

            if Is_Function (Subpr)
              and then not Has_Scalar_Result (Subpr)
            then
               Method.Add_Formal_Argument
                 ("Result", Out_Argument,
                  IDL.Types.Get_Ada_Name (Get_Return_Type (Subpr)));
               if Has_Count_Type (Get_Return_Type (Subpr)) then
                  Method.Add_Formal_Argument
                    ("Last", Out_Argument,
                     IDL.Types.Get_Ada_Name
                       (Get_Count_Type (Get_Return_Type (Subpr))));
               end if;
            end if;
         end;

         Pkg.Add_Separator;
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

      Add_Context (Item, "Rose.Capabilities");

--      Client.With_Package ("Rose.Words", Body_With => True);

      for I in 1 .. Get_Num_Contexts (Item) loop
         Client.With_Package (Get_Context (Item, I));
      end loop;

      Client.With_Package ("Rose.Invocation");
      Client.With_Package ("Rose.System_Calls", Body_With => True);
      Client.With_Package ("Rose.Environment", Body_With => True);

      With_Packages (Item, Client, False);

      Generate_Interface_Record
        (Pkg    => Client,
         Item   => Item,
         Client => True);

      Client.Append_To_Body
        (Syn.Declarations.New_Object_Declaration
           ("Last_Error",
            Syn.Named_Subtype ("Rose.Invocation.Invocation_Error"),
            Syn.Object ("Rose.Invocation.OK")));

      declare
         Block : Syn.Blocks.Block_Type;

         procedure Add_Initialiser
           (Interface_Subprogram : IDL.Syntax.IDL_Subprogram);

         ---------------------
         -- Add_Initialiser --
         ---------------------

         procedure Add_Initialiser
           (Interface_Subprogram : IDL.Syntax.IDL_Subprogram)
         is
         begin
            Block.Append
              (Syn.Statements.New_Assignment_Statement
                 (Target => "Client." & Get_Ada_Name (Interface_Subprogram),
                  Value  =>
                    Syn.Object (Get_Ada_Name (Interface_Subprogram))));
         end Add_Initialiser;

      begin

         Block.Append
           (Syn.Statements.New_Assignment_Statement
              (Target => "Client.Is_Open",
               Value  => Syn.Literal (False)));
         Scan_Subprograms (Item, True, Add_Initialiser'Access);

         Block.Append
           (Syn.Statements.New_Assignment_Statement
              (Target => "Client.Is_Open",
               Value  => Syn.Literal (True)));

         declare
            Open_Procedure : Syn.Declarations.Subprogram_Declaration'Class :=
                               Syn.Declarations.New_Procedure
                                 ("Open_Cap_Set",
                                  Syn.Declarations.New_Formal_Argument
                                    ("Client",
                                     Syn.Declarations.Out_Argument,
                                     Syn.Named_Subtype
                                       (Interface_Name & "_Client")),
                                  Block);

            procedure Add_Argument
              (Interface_Subprogram : IDL.Syntax.IDL_Subprogram);

            ------------------
            -- Add_Argument --
            ------------------

            procedure Add_Argument
              (Interface_Subprogram : IDL.Syntax.IDL_Subprogram)
            is
            begin
               Open_Procedure.Add_Formal_Argument
                 (Syn.Declarations.New_Formal_Argument
                    (Get_Ada_Name (Interface_Subprogram),
                     Syn.Named_Subtype ("Rose.Capabilities.Capability")));
            end Add_Argument;

         begin
            Scan_Subprograms (Item, True, Add_Argument'Access);
            Client.Add_Separator;
            Client.Append (Open_Procedure);
         end;

         Generate_Open_Interface (Client, Item);

      end;

      declare
         Block : Syn.Blocks.Block_Type;

         procedure Add_Deleted_Cap
           (Interface_Subprogram : IDL.Syntax.IDL_Subprogram);

         ---------------------
         -- Add_Deleted_Cap --
         ---------------------

         procedure Add_Deleted_Cap
           (Interface_Subprogram : IDL.Syntax.IDL_Subprogram)
         is
         begin
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Rose.System_Calls.Send_Cap",
                  Syn.Object ("Params"),
                  Syn.Object
                    ("Client." & Get_Ada_Name (Interface_Subprogram))));
         end Add_Deleted_Cap;

      begin

         Block.Add_Declaration
           (Syn.Declarations.New_Object_Declaration
              (Identifiers => Syn.Declarations.Identifier ("Params"),
               Is_Aliased  => True,
               Is_Constant => False,
               Is_Deferred => False,
               Object_Type =>
                 Syn.Named_Subtype
                   ("Rose.Invocation.Invocation_Record")));

         Block.Append
           (Syn.Statements.New_Assignment_Statement
              (Target => "Client.Is_Open",
               Value  => Syn.Literal (False)));

         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Rose.System_Calls.Initialize_Send",
               Syn.Object ("Params"),
               Syn.Object ("Rose.Environment.Standard_Delete_Cap")));

         Scan_Subprograms (Item, True, Add_Deleted_Cap'Access);

         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Rose.System_Calls.Invoke_Capability",
               Syn.Object ("Params")));

         Client.Add_Separator;
         Client.Append (Syn.Declarations.New_Procedure
                        ("Close",
                           Syn.Declarations.New_Formal_Argument
                             ("Client",
                              Syn.Declarations.Inout_Argument,
                              Syn.Named_Subtype
                                (Interface_Name & "_Client")),
                           Block));
      end;

      for I in 1 .. Get_Num_Inherited (Item) loop
         Generate_Inherited_Overrides (Client, Item,
                                       Get_Inherited (Item, I));
      end loop;

      for I in Subprs'Range loop
         Generate_Client_Override (Client, Item, Subprs (I), Null_Interface);
      end loop;

      for I in Subprs'Range loop
         Generate_Cap_Query (Client, Item, Subprs (I), Null_Interface);
      end loop;

      declare
         Block : Syn.Blocks.Block_Type;
      begin
         Block.Add_Declaration
           (Syn.Declarations.Use_Package ("Rose.Invocation"));
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.Operator
                   ("/=", Syn.Object ("Last_Error"), Syn.Object ("OK"))));
         Client.Append
           (Syn.Declarations.New_Function
              ("Has_Error", Syn.Named_Subtype ("Boolean"),
               Block));
      end;

      declare
         Block : Syn.Blocks.Block_Type;
      begin
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Object ("Last_Error")));
         Client.Append
           (Syn.Declarations.New_Function
              ("Get_Last_Error",
               Syn.Named_Subtype ("Rose.Invocation.Invocation_Error"),
               Block));
      end;

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
         Generate_Client_Override (Pkg, Item, Subprs (I), Parent);
      end loop;
   end Generate_Inherited_Overrides;

   --------------------------------
   -- Generate_Interface_Package --
   --------------------------------

   procedure Generate_Interface_Package (Item : IDL.Syntax.IDL_Interface) is
      use IDL.Syntax, IDL.Identifiers;
      Interface_Name : constant String :=
                         To_Ada_Name (Get_Name (Item));
      Package_Name   : constant String :=
                         Interface_Name;
      IF_Pkg         : Syn.Declarations.Package_Type :=
                         Syn.Declarations.New_Package_Type
                           ("Rose.Interfaces." & Package_Name);
      Subprs         : constant IDL_Subprogram_Array :=
                         Get_Subprograms (Item);
      First          : Boolean := True;
   begin

--        for I in 1 .. Get_Num_Contexts (Item) loop
--           IF_Pkg.With_Package (Get_Context (Item, I));
--        end loop;

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
               elsif Is_Range_Type (New_Type) then
                  IF_Pkg.Append
                    (Syn.Declarations.New_Full_Type_Declaration
                       (Get_Object_Ada_Name (Item, I),
                        Syn.Types.New_Range_Definition
                          (Integer'Image (Get_Low (New_Type)),
                           Integer'Image (Get_High (New_Type)))));
               else
                  IF_Pkg.Append
                    (Syn.Declarations.New_Full_Type_Declaration
                       (Get_Object_Ada_Name (Item, I),
                        Syn.New_Derived_Type
                          (IDL.Types.Get_Ada_Name (New_Type))));
                  if Is_Word_Type (New_Type) then
                     IF_Pkg.With_Package ("Rose.Words");
                  end if;

               end if;
            end;
         end if;
      end loop;

      declare
         procedure Declare_Interface_Endpoint
           (For_Interface : IDL_Interface);

         --------------------------------
         -- Declare_Interface_Endpoint --
         --------------------------------

         procedure Declare_Interface_Endpoint
           (For_Interface : IDL_Interface)
         is
         begin
            IF_Pkg.Append
              (Syn.Declarations.New_Constant_Declaration
                 (Name        => Get_Ada_Name (For_Interface) & "_Interface",
                  Object_Type => "Rose.Objects.Endpoint_Id",
                  Value       =>
                    Syn.Object
                      (IDL.Endpoints.Endpoint_Id
                           ("interface",
                            Get_Ada_Name (For_Interface) & "_Interface"))));
         end Declare_Interface_Endpoint;

      begin
         Scan_Ancestors (Item, True, Declare_Interface_Endpoint'Access);
      end;

      for Subpr of Subprs loop

         if First then
            IF_Pkg.With_Package ("Rose.Objects");
            First := False;
         end if;

         IF_Pkg.Append
           (Syn.Declarations.New_Constant_Declaration
              (Name        => Get_Name (Subpr) & "_Endpoint",
               Object_Type => "Rose.Objects.Endpoint_Id",
               Value       =>
                 Syn.Object
                   (IDL.Endpoints.Endpoint_Id
                        (Interface_Name, Get_Name (Subpr)))));
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
                           & (if Client then "Client" else "Server");
      Interface_Record : Syn.Types.Record_Type_Definition;
   begin
      Interface_Record.Add_Component
        ("Is_Open", "Boolean", "False");

      declare
         procedure Add_Cap (Subpr : IDL_Subprogram);

         -------------
         -- Add_Cap --
         -------------

         procedure Add_Cap (Subpr : IDL_Subprogram) is
         begin
            Interface_Record.Add_Component
              (Get_Ada_Name (Subpr), "Rose.Capabilities.Capability", "0");
         end Add_Cap;

      begin
         Scan_Subprograms (Item, True, Add_Cap'Access);
      end;

      declare
         Type_Dec : Syn.Declaration'Class :=
                      Syn.Declarations.New_Private_Type_Declaration
                        (Record_Name,
                         Interface_Record);
      begin
         Type_Dec.Set_Private_Spec;
         Pkg.Append (Type_Dec);
      end;

   end Generate_Interface_Record;

   -------------------------------
   -- Generate_Kernel_Interface --
   -------------------------------

   procedure Generate_Kernel_Interface (Item : IDL.Syntax.IDL_Interface) is
   begin
      Generate_Interface_Package (Item);
      Generate_Client_Package (Item);
      Generate_Server_Package (Item);
   end Generate_Kernel_Interface;

   -----------------------------
   -- Generate_Open_Interface --
   -----------------------------

   procedure Generate_Open_Interface
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface)
   is
      use IDL.Syntax;
      Block : Syn.Blocks.Block_Type;
      Interface_Name : constant String :=
                         IDL.Syntax.Get_Ada_Name (Item);
      Subprs         : constant IDL_Subprogram_Array :=
                         Get_Subprograms (Item);
   begin

      Block.Add_Declaration
        (Syn.Declarations.New_Object_Declaration
           (Identifiers => Syn.Declarations.Identifier ("Params"),
            Is_Aliased  => True,
            Is_Constant => False,
            Is_Deferred => False,
            Object_Type =>
              Syn.Named_Subtype
                ("Rose.Invocation.Invocation_Record")));

      Block.Add_Statement
        (Syn.Statements.New_Assignment_Statement
           ("Client.Is_Open", Syn.Literal (False)));

      Block.Add_Statement
        (Syn.Statements.New_Procedure_Call_Statement
           ("Rose.System_Calls.Initialize_Send",
            Syn.Object ("Params"),
            Syn.Object ("Interface_Cap")));

      Block.Add_Statement
        (Syn.Statements.New_Procedure_Call_Statement
           ("Rose.System_Calls.Receive_Caps",
            Syn.Object ("Params"),
            Syn.Literal (Subprs'Length)));

      Block.Add_Statement
        (Syn.Statements.New_Procedure_Call_Statement
           ("Rose.System_Calls.Invoke_Capability",
            Syn.Object ("Params")));

      for I in Subprs'Range loop
         Block.Add_Statement
           (Syn.Statements.New_Assignment_Statement
              ("Client." & Get_Ada_Name (Subprs (I)),
               Syn.Expressions.New_Function_Call_Expression
                 ("Params.Caps", Syn.Literal (I - 1))));
      end loop;

      Block.Add_Statement
        (Syn.Statements.New_Assignment_Statement
           ("Client.Is_Open", Syn.Literal (True)));

      declare
         Open_Procedure : constant
           Syn.Declarations.Subprogram_Declaration'Class :=
             Syn.Declarations.New_Procedure
               ("Open",
                Syn.Declarations.New_Formal_Argument
                  ("Client",
                   Syn.Declarations.Out_Argument,
                   Syn.Named_Subtype
                     (Interface_Name & "_Client")),
                Syn.Declarations.New_Formal_Argument
                  ("Interface_Cap",
                   Syn.Named_Subtype
                     ("Rose.Capabilities.Capability")),
                Block);

      begin
         Pkg.Add_Separator;
         Pkg.Append (Open_Procedure);
      end;

   end Generate_Open_Interface;

   ------------------------------
   -- Generate_Server_Override --
   ------------------------------

   procedure Generate_Server_Override
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Parent  : IDL.Syntax.IDL_Interface)
   is
      pragma Unreferenced (Parent, Item);
      use IDL.Syntax, IDL.Identifiers;
      Name           : constant String :=
                         "Handle_" & To_Ada_Name (Get_Name (Subpr));
      --  Interface_Name : constant String := Get_Ada_Name (Item);
      Block          : Syn.Blocks.Block_Type;
   begin

      Copy_Server_Invocation (Block, Subpr);

      declare
         use Syn.Declarations;
         Method : Subprogram_Declaration'Class :=
                    New_Procedure (Name, Block);
      begin
         Method.Add_Formal_Argument
           ("Parameters",
            Inout_Argument,
            "Rose.Invocation.Invocation_Record");

         Pkg.Add_Separator;
         Pkg.Append_To_Body (Method);

      end;

   end Generate_Server_Override;

   -----------------------------
   -- Generate_Server_Package --
   -----------------------------

   procedure Generate_Server_Package (Item : IDL.Syntax.IDL_Interface) is
      use IDL.Syntax, IDL.Identifiers;
      Server_Pkg     : Syn.Declarations.Package_Type;
      Interface_Name : constant String :=
                         To_Ada_Name (Get_Name (Item));
      Package_Name   : constant String :=
                         Interface_Name & ".Server";
      Subprs         : constant IDL_Subprogram_Array :=
                         Get_Subprograms (Item);
   begin

      Server_Pkg :=
        Syn.Declarations.New_Package_Type
          ("Rose.Interfaces." & Package_Name);

      Server_Pkg.With_Package ("Rose.Capabilities");
      Server_Pkg.With_Package ("Rose.Server");
      Server_Pkg.With_Package ("Rose.System_Calls.Server",
                               Body_With => True);

      Server_Pkg.With_Package ("Rose.Invocation",
                               Body_With => True, Use_Package => True);
      Server_Pkg.With_Package ("Rose.System_Calls",
                               Body_With => True, Use_Package => True);

      if False then
         for I in 1 .. Get_Num_Contexts (Item) loop
            Server_Pkg.With_Package
              (Get_Context (Item, I), Body_With => True);
         end loop;
      end if;

      With_Packages (Item, Server_Pkg, True);

      for I in Subprs'Range loop
         Server_Pkg.Append_To_Body
           (Syn.Declarations.New_Object_Declaration
              ("Local_" & Get_Ada_Name (Subprs (I)),
               Get_Ada_Name (Subprs (I)) & "_Handler"));
         Server_Pkg.Append_To_Body
           (Syn.Declarations.New_Object_Declaration
              (Get_Ada_Name (Subprs (I)) & "_Cap",
               "Rose.Capabilities.Capability"));
      end loop;

      declare
         procedure Generate_Interface_Request
           (For_Interface : IDL_Interface);

         --------------------------------
         -- Generate_Interface_Request --
         --------------------------------

         procedure Generate_Interface_Request
           (For_Interface : IDL_Interface)
         is
            Block : Syn.Blocks.Block_Type;

            procedure Send_Subprogram_Cap (Item : IDL_Subprogram);

            -------------------------
            -- Send_Subprogram_Cap --
            -------------------------

            procedure Send_Subprogram_Cap (Item : IDL_Subprogram) is
            begin
               Block.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Rose.System_Calls.Send_Cap",
                     Syn.Object ("Parameters"),
                     Syn.Object (Get_Ada_Name (Item) & "_Cap")));
            end Send_Subprogram_Cap;

         begin

            Scan_Subprograms (For_Interface, True, Send_Subprogram_Cap'Access);

            declare
               use Syn.Declarations;
               Name : constant String :=
                        "Handle_Get_"
                        & Get_Ada_Name (For_Interface);
               Method : Subprogram_Declaration'Class :=
                          New_Procedure (Name, Block);
            begin
               Method.Add_Formal_Argument
                 ("Parameters",
                  Inout_Argument,
                  "Rose.Invocation.Invocation_Record");

               Server_Pkg.Add_Separator;
               Server_Pkg.Append_To_Body (Method);

            end;
         end Generate_Interface_Request;

      begin
         Scan_Ancestors (Item, True, Generate_Interface_Request'Access);
      end;

      for I in Subprs'Range loop
         Generate_Server_Subprogram_Type
           (Server_Pkg, Item, Subprs (I), Null_Interface);
      end loop;

      for I in Subprs'Range loop
         Generate_Server_Override
           (Server_Pkg, Item, Subprs (I), Null_Interface);
      end loop;

      declare
         Block       : Syn.Blocks.Block_Type;

         procedure Create_Endpoint (Subpr : IDL_Subprogram);
         procedure Register_Endpoint (Subpr : IDL_Subprogram);
         procedure Save_Handler (Subpr : IDL_Subprogram);

         procedure Register_Interface (Item : IDL_Interface);

         ---------------------
         -- Create_Endpoint --
         ---------------------

         procedure Create_Endpoint (Subpr : IDL_Subprogram) is
         begin
            Block.Append
              (Syn.Statements.New_Assignment_Statement
                 (Get_Ada_Name (Subpr) & "_Cap",
                  Syn.Expressions.New_Function_Call_Expression
                    ("Rose.System_Calls.Server.Create_Endpoint",
                     Syn.Literal (1),
                     Syn.Object (Get_Ada_Name (Subpr) & "_Endpoint"))));
         end Create_Endpoint;

         -----------------------
         -- Register_Endpoint --
         -----------------------

         procedure Register_Endpoint (Subpr : IDL_Subprogram) is
         begin
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Rose.Server.Register_Handler",
                  Syn.Object ("Server_Context"),
                  Syn.Object (Get_Ada_Name (Subpr) & "_Endpoint"),
                  Syn.Object
                    ("Handle_" & Get_Ada_Name (Subpr) & "'Access")));
         end Register_Endpoint;

         ------------------------
         -- Register_Interface --
         ------------------------

         procedure Register_Interface (Item : IDL_Interface) is
         begin
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Rose.Server.Register_Handler",
                  Syn.Object ("Server_Context"),
                  Syn.Object (Get_Ada_Name (Item) & "_Interface"),
                  Syn.Object
                    ("Handle_Get_" & Get_Ada_Name (Item) & "'Access")));
         end Register_Interface;

         ------------------
         -- Save_Handler --
         ------------------

         procedure Save_Handler (Subpr : IDL_Subprogram) is
         begin
            Block.Append
              (Syn.Statements.New_Assignment_Statement
                 (Target => "Local_" & Get_Ada_Name (Subpr),
                  Value  => Syn.Object (Get_Ada_Name (Subpr))));
         end Save_Handler;

      begin

         Scan_Subprograms (Item, True, Save_Handler'Access);
         Scan_Subprograms (Item, True, Create_Endpoint'Access);

         Scan_Ancestors (Item, True, Register_Interface'Access);

         Scan_Subprograms (Item, True, Register_Endpoint'Access);

         declare
            Proc_Name : constant String := "Create_Server";
            Proc      : Syn.Declarations.Subprogram_Declaration'Class :=
                          Syn.Declarations.New_Procedure
                            (Proc_Name, Block);

            procedure Add_Endpoint_Handler (Subpr : IDL_Subprogram);

            --------------------------
            -- Add_Endpoint_Handler --
            --------------------------

            procedure Add_Endpoint_Handler (Subpr : IDL_Subprogram) is
            begin
               Proc.Add_Formal_Argument
                 (Arg_Name       => Get_Ada_Name (Subpr),
                  Arg_Type       => Get_Ada_Name (Subpr) & "_Handler");
            end Add_Endpoint_Handler;

         begin
            Proc.Add_Formal_Argument
              (Arg_Name       => "Server_Context",
               Arg_Mode       => Syn.Declarations.Inout_Argument,
               Arg_Type       => "Rose.Server.Server_Context");
            Scan_Subprograms (Item, True, Add_Endpoint_Handler'Access);

            Server_Pkg.Add_Separator;
            Server_Pkg.Append (Proc);
         end;
      end;

      declare
         Writer : Syn.File_Writer.File_Writer;
      begin
         Server_Pkg.Write (Writer);
      end;

   end Generate_Server_Package;

   ------------------------------
   -- Generate_Server_Override --
   ------------------------------

   procedure Generate_Server_Subprogram_Type
     (Pkg     : in out Syn.Declarations.Package_Type'Class;
      Item    : IDL.Syntax.IDL_Interface;
      Subpr   : IDL.Syntax.IDL_Subprogram;
      Parent  : IDL.Syntax.IDL_Interface)
   is
      pragma Unreferenced (Parent, Item);
      use IDL.Syntax, IDL.Identifiers;
      Name           : constant String := To_Ada_Name (Get_Name (Subpr));
      Handle_Name    : constant String :=
                         "Handle_" & Name with Unreferenced;
      Type_Name      : constant String :=
                         Name & "_Handler";
      Block          : Syn.Blocks.Block_Type;
   begin

      Block.Add_Declaration
        (Syn.Declarations.New_Object_Declaration
           (Identifiers => Syn.Declarations.Identifier ("Params"),
            Is_Aliased  => True,
            Is_Constant => False,
            Is_Deferred => False,
            Object_Type =>
              Syn.Named_Subtype
                ("Rose.Invocation.Invocation_Record")));

      Initialise_Invocation (Block, Subpr);

      Block.Add_Statement
        (Syn.Statements.New_Procedure_Call_Statement
           ("Rose.System_Calls.Invoke_Capability",
            Syn.Object ("Params")));

      Check_Error (Block);

      Copy_Invocation_Result (Block, Subpr);

      declare
         use Syn.Declarations;
         Method : Subprogram_Declaration'Class :=
                    (if not Is_Function (Subpr)
                     or else not Has_Scalar_Result (Subpr)
                     then New_Procedure (Name, Block)
                     else New_Function
                       (Name,
                        IDL.Types.Get_Ada_Name
                          (Get_Return_Type (Subpr)),
                        Block));
      begin
         Method.Add_Formal_Argument
           ("Id", "Rose.objects.Capability_Identifier");

         declare
            use IDL.Types;
            Args : constant IDL_Argument_Array :=
                     Get_Arguments (Subpr);
         begin
            for Arg of Args loop
               if IDL.Types.Is_Interface (Get_Type (Arg)) then
                  Method.Add_Formal_Argument
                    (To_Ada_Name (Get_Name (Arg)),
                     Get_Syn_Mode (Arg),
                     "Rose.Capabilities.Capability");
               else
                  Method.Add_Formal_Argument
                    (To_Ada_Name (Get_Name (Arg)),
                     Get_Syn_Mode (Arg),
                     IDL.Types.Get_Ada_Name
                       (Get_Type (Arg)));
               end if;
            end loop;

            if Is_Function (Subpr)
              and then not Has_Scalar_Result (Subpr)
            then
               Method.Add_Formal_Argument
                 ("Result", Out_Argument,
                  IDL.Types.Get_Ada_Name (Get_Return_Type (Subpr)));
               if Has_Count_Type (Get_Return_Type (Subpr)) then
                  Method.Add_Formal_Argument
                    ("Last", Out_Argument,
                     IDL.Types.Get_Ada_Name
                       (Get_Count_Type (Get_Return_Type (Subpr))));
               end if;
            end if;
         end;

         Pkg.Add_Separator;
         Pkg.Append
           (Syn.Declarations.New_Full_Type_Declaration
              (Type_Name,
               Syn.Types.New_Subprogram_Type_Definition
                 (Method)));

      end;

   end Generate_Server_Subprogram_Type;

   ---------------------------
   -- Initialise_Invocation --
   ---------------------------

   procedure Initialise_Invocation
     (Block : in out Syn.Blocks.Block_Type'Class;
      Subpr : IDL.Syntax.IDL_Subprogram)
   is
      use IDL.Syntax, IDL.Types;
      Args       : constant IDL_Argument_Array := Get_Arguments (Subpr);
      Have_Non_Scalar : Boolean := False;
      Recv_Buffer     : Boolean := False;
      Recv_Words      : Natural := 0;
      Recv_Caps       : Natural := 0;
      Buffer_Arg      : Natural := 0;

--        function Trim (S : String) return String
--        is (Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));
--
   begin

      Block.Add_Statement
        (Syn.Statements.New_Procedure_Call_Statement
           ("Rose.System_Calls.Initialize_Send",
            Syn.Object ("Params"),
            Syn.Object ("Item." & Get_Ada_Name (Subpr))));

      for I in Args'Range loop
         declare
            Mode     : constant IDL_Argument_Mode := Get_Mode (Args (I));
            Arg_Type : constant IDL_Type := Get_Type (Args (I));
            Arg_Name : constant String := Get_Ada_Name (Args (I));
         begin

            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("--  " & Get_Ada_Name (Args (I))
                  & " "
                  & (case Mode is
                       when In_Argument    => "in",
                       when Inout_Argument => "in out",
                       when Out_Argument   => "out")
                  & " "
                  & (if Is_Scalar (Arg_Type)
                    then "scalar" else "composite")));

            if Mode = Out_Argument
              and then not Is_Scalar (Arg_Type)
            then
               Recv_Buffer := True;
               Buffer_Arg := I;
            end if;

            if Mode = In_Argument
              or else Mode = Inout_Argument
            then
               if Is_Capability (Arg_Type) then
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Rose.System_Calls.Send_Cap",
                        Syn.Object ("Params"),
                        Syn.Object (Get_Ada_Name (Args (I)))));
               elsif not Is_Scalar (Arg_Type) then
                  if Have_Non_Scalar then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "in '" & Get_Ada_Name (Subpr)
                        & "': only one non-scalar argument allowed");
                  end if;

                  Have_Non_Scalar := True;

                  if Mode = In_Argument
                    or else Mode = Inout_Argument
                  then
                     if Is_String (Arg_Type) then
                        Block.Append
                          (Syn.Statements.New_Procedure_Call_Statement
                             ("Rose.System_Calls.Send_Text",
                              Syn.Object ("Params"),
                              Syn.Object (Get_Ada_Name (Args (I)))));
                     elsif Is_Capability_Array (Arg_Type) then
                        declare
                           use Syn, Syn.Statements;
                           Assign : constant Statement'Class :=
                                      New_Procedure_Call_Statement
                                        ("Rose.System_Calls.Send_Cap",
                                         Object ("Params"),
                                         Object ("Cap"));
                           Seq    : Sequence_Of_Statements;
                        begin
                           Seq.Append (Assign);
                           Block.Append
                             (Syn.Statements.Iterate
                                (Loop_Variable  => "Cap",
                                 Container_Name => Arg_Name,
                                 Iterate_Body   => Seq));
                        end;
                     else
                        Block.Append
                          (Syn.Statements.New_Procedure_Call_Statement
                             ("Rose.System_Calls.Send_Storage_Array",
                              Syn.Object ("Params"),
                              Syn.Object (Arg_Name),
                              Syn.Literal (Mode = Inout_Argument)));
                     end if;
                  end if;

               else
                  if Is_Word_Type (Arg_Type)
                    or else Is_Integer_Type (Arg_Type)
                  then
                     Block.Append
                       (Syn.Statements.New_Procedure_Call_Statement
                          ("Rose.System_Calls.Send_Word",
                           Syn.Object ("Params"),
                           Syn.Object (Get_Ada_Name (Args (I)))));
                  elsif Get_Size (Arg_Type) > Default_Size then
                     Block.Append
                       (Syn.Statements.New_Procedure_Call_Statement
                          ("Rose.System_Calls.Send_Word",
                           Syn.Object ("Params"),
                           Syn.Expressions.New_Function_Call_Expression
                             ("Rose.Words.Word_64",
                              Syn.Object (Get_Ada_Name (Args (I))))));
                  else
                     Block.Append
                       (Syn.Statements.New_Procedure_Call_Statement
                          ("Rose.System_Calls.Send_Word",
                           Syn.Object ("Params"),
                           Syn.Expressions.New_Function_Call_Expression
                             ("Rose.Words.Word",
                              Syn.Object (Get_Ada_Name (Args (I))))));
                  end if;
               end if;
               if Mode = Out_Argument or else Mode = Inout_Argument then
                  Recv_Words := Recv_Words + 1;
               end if;
            end if;

         end;
      end loop;

      if Is_Function (Subpr) then
         declare
            Result : constant IDL.Types.IDL_Type :=
                       Get_Return_Type (Subpr);
         begin
            if Is_Interface (Result) then
               Recv_Caps := Recv_Caps + 8;
            elsif Is_Scalar (Result) then
               Recv_Words := Recv_Words + 1;
            else
               Recv_Buffer := True;
               Buffer_Arg := 0;
            end if;
         end;
      end if;

      if Recv_Words > 0 then
         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Rose.System_Calls.Receive_Words",
               Syn.Object ("Params"),
               Syn.Literal (Recv_Words)));
      end if;

      if Recv_Caps > 0 then
         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Rose.System_Calls.Receive_Caps",
               Syn.Object ("Params"),
               Syn.Literal (Recv_Caps)));
      end if;

      if Recv_Buffer then
         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Rose.System_Calls.Receive_Buffer",
               Syn.Object ("Params"),
               Syn.Object
                 ((if Buffer_Arg = 0 then "Result"
                  else Get_Ada_Name (Args (Buffer_Arg)))
                    & "'Length")));
      end if;

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
            declare
               Pkg_Name : constant String :=
                            IDL.Types.Get_Ada_Package (Ref);
            begin
               if not Server then
                  Pkg.With_Package (Pkg_Name, Body_With => True);
               end if;
            end;
         else
            declare
               Pkg_Name : constant String :=
                            IDL.Types.Get_Ada_Package (Ref);
            begin
               if Pkg_Name /= ""  then
                  Pkg.With_Package (Pkg_Name);
               end if;
            end;
            if Server
              and then IDL.Types.Is_String (Ref)
            then
               Pkg.With_Package ("System.Storage_Elements",
                                 Body_With => True);
            end if;
         end if;
      end Check_Type;

   begin
      Scan_Subprograms (Top_Interface, True, Add_Dependent_Withs'Access);
   end With_Packages;

end IDL.Generate_Kernel;
