with IDL.Types;

package IDL.Procs is

   type IDL_Procedure is private;

   function Standard_Name (Proc : IDL_Procedure) return String;
   function Declared_Name (Proc : IDL_Procedure) return String;

   procedure Scan_Arguments
     (Proc : IDL_Procedure;
      Process : not null access
        procedure (Arg_Name : String;
                   Arg_Type : IDL.Types.IDL_Type));

   procedure Scan_Locals
     (Proc    : IDL_Procedure;
      Process : not null access
        procedure (Local_Name : String;
                   Local_Type : IDL.Types.IDL_Type));

   function New_Procedure_Declaration
     (Name : String)
      return IDL_Procedure;

   procedure Add_Argument
     (Proc     : IDL_Procedure;
      Name     : String;
      Arg_Type : IDL.Types.IDL_Type);

   procedure Add_Local
     (Proc       : IDL_Procedure;
      Name       : String;
      Local_Type : IDL.Types.IDL_Type);

   type IDL_Expression is private;

   function Is_Integer_Literal (Expr : IDL_Expression) return Boolean;
   function Is_List (Expr : IDL_Expression) return Boolean;
   function Is_Object_Name (Expr : IDL_Expression) return Boolean;
   function Is_String_Literal (Expr : IDL_Expression) return Boolean;

   function Get_Integer_Value (Expr : IDL_Expression) return Integer;
   function Get_String_Value (Expr : IDL_Expression) return String;
   function Get_Object_Name (Expr : IDL_Expression) return String;

   function Get_Length (Expr : IDL_Expression) return Natural;

   function New_Object_Expression
     (Object_Name : String)
      return IDL_Expression;

   function New_String_Expression
     (Literal : String)
      return IDL_Expression;

   function New_Integer_Expression
     (Value : Integer)
      return IDL_Expression;

   function New_List_Expression return IDL_Expression;
   procedure Append
     (List    : IDL_Expression;
      Element : IDL_Expression);

   function New_Operator_Expression
     (Operator : String;
      Left, Right : IDL_Expression)
      return IDL_Expression;

   function Evaluate_Static_Expression
     (Expr : IDL_Expression)
      return IDL_Expression;

   procedure Scan
     (Expression : IDL_Expression;
      Process    : not null access
        procedure (Expr : IDL_Expression));

   type IDL_Command is private;

   procedure Add_Command
     (Proc    : IDL_Procedure;
      Command : IDL_Command);

   function New_Invoke_Command
     (Cap_Name : String;
      Endpoint : Positive)
      return IDL_Command;

   procedure Add_Aspect
     (Command      : IDL_Command;
      Aspect_Name  : String;
      Aspect_Value : IDL_Expression);

   type IDL_Aspects is private;

   procedure Scan_Aspects
     (Aspects : IDL_Aspects;
      Process : not null access
        procedure (Name    : String;
                   Value   : IDL_Expression));

   function Has_Aspect
     (Aspects : IDL_Aspects;
      Name    : String)
      return Boolean;

   function Aspect
     (Aspects : IDL_Aspects;
      Name    : String)
      return IDL_Expression;

   procedure Scan_Commands
     (Proc    : IDL_Procedure;
      Process_Invoke : not null access
        procedure (Cap_Name : String;
                   Endpoint : Positive;
                   Aspects  : IDL_Aspects));

private

   type IDL_Expression_Record;
   type IDL_Expression is access IDL_Expression_Record;

   type IDL_Command_Record;
   type IDL_Command is access IDL_Command_Record;

   type IDL_Procedure_Record;
   type IDL_Procedure is access IDL_Procedure_Record;

   type IDL_Aspects_Record;
   type IDL_Aspects is access IDL_Aspects_Record;

end IDL.Procs;
