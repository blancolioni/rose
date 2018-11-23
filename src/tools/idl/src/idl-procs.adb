with Ada.Containers.Vectors;

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Characters.Handling;          use Ada.Characters.Handling;

with WL.String_Maps;

package body IDL.Procs is

   type Declaration is
      record
         Declared_Name : Unbounded_String;
         Standard_Name : Unbounded_String;
         Typ           : IDL.Types.IDL_Type;
      end record;

   package Declaration_Vectors is
     new Ada.Containers.Vectors (Positive, Declaration);

   type Aspect_Record is
      record
         Name : Unbounded_String;
         Value : IDL_Expression;
      end record;

   package Aspect_Vectors is
     new Ada.Containers.Vectors (Positive, Aspect_Record);

   type IDL_Aspects_Record is
      record
         Vector : Aspect_Vectors.Vector;
      end record;

   type Command_Type is (Invoke);

   type IDL_Command_Record (Command : Command_Type) is
      record
         Aspects : IDL_Aspects;
         case Command is
            when Invoke =>
               Cap_Name : Unbounded_String;
               Endpoint : Positive;
         end case;
      end record;

   package Command_Vectors is
     new Ada.Containers.Vectors (Positive, IDL_Command);

   package Expression_Vectors is
     new Ada.Containers.Vectors (Positive, IDL_Expression);

   type Expression_Type is (String_Expr, Object_Expr, Integer_Expr,
                            Operator_Expr, List_Expr);

   type IDL_Expression_Record (Expr : Expression_Type) is
      record
         case Expr is
            when String_Expr =>
               String_Value : Unbounded_String;
            when Object_Expr =>
               Object_Name  : Unbounded_String;
            when Integer_Expr =>
               Integer_Value : Integer;
            when Operator_Expr =>
               Operator_Name : Unbounded_String;
               Left, Right   : IDL_Expression;
            when List_Expr =>
               List_Value    : Expression_Vectors.Vector;
         end case;
      end record;

   type IDL_Procedure_Record is
      record
         Declared_Name : Unbounded_String;
         Standard_Name : Unbounded_String;
         Arguments     : Declaration_Vectors.Vector;
         Locals        : Declaration_Vectors.Vector;
         Commands      : Command_Vectors.Vector;
      end record;

   package Object_Value_Maps is
     new WL.String_Maps (IDL.Procs.IDL_Expression,
                         IDL.Procs."=");

   Table     : Object_Value_Maps.Map;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Proc     : IDL_Procedure;
      Name     : String;
      Arg_Type : IDL.Types.IDL_Type)
   is
   begin
      Proc.Arguments.Append
        (Declaration'
           (Declared_Name => To_Unbounded_String (Name),
            Standard_Name => To_Unbounded_String (To_Lower (Name)),
            Typ           => Arg_Type));
   end Add_Argument;

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Aspect
     (Command      : IDL_Command;
      Aspect_Name  : String;
      Aspect_Value : IDL_Expression)
   is
   begin
      Command.Aspects.Vector.Append
        (Aspect_Record'
           (Name  => To_Unbounded_String (Aspect_Name),
            Value => Aspect_Value));
   end Add_Aspect;

   -----------------
   -- Add_Command --
   -----------------

   procedure Add_Command
     (Proc    : IDL_Procedure;
      Command : IDL_Command)
   is
   begin
      Proc.Commands.Append (Command);
   end Add_Command;

   ---------------
   -- Add_Local --
   ---------------

   procedure Add_Local
     (Proc       : IDL_Procedure;
      Name       : String;
      Local_Type : IDL.Types.IDL_Type)
   is
   begin
      Proc.Locals.Append
        (Declaration'
           (Declared_Name => To_Unbounded_String (Name),
            Standard_Name => To_Unbounded_String (To_Lower (Name)),
            Typ           => Local_Type));
   end Add_Local;

   ------------
   -- Append --
   ------------

   procedure Append
     (List    : IDL_Expression;
      Element : IDL_Expression)
   is
   begin
      List.List_Value.Append (Element);
   end Append;

   ------------
   -- Aspect --
   ------------

   function Aspect
     (Aspects : IDL_Aspects;
      Name    : String)
      return IDL_Expression
   is
   begin
      for Item of Aspects.Vector loop
         if Item.Name = Name then
            return Item.Value;
         end if;
      end loop;
      return null;
   end Aspect;

   -------------------
   -- Declared_Name --
   -------------------

   function Declared_Name (Proc : IDL_Procedure) return String is
   begin
      return To_String (Proc.Declared_Name);
   end Declared_Name;

   --------------------------------
   -- Evaluate_Static_Expression --
   --------------------------------

   function Evaluate_Static_Expression
     (Expr : IDL_Expression)
      return IDL_Expression
   is
   begin
      case Expr.Expr is
         when String_Expr =>
            return Expr;
         when Object_Expr =>
            if Table.Contains (To_String (Expr.Object_Name)) then
               return Table.Element (To_String (Expr.Object_Name));
            else
               return Expr;
            end if;
         when Integer_Expr =>
            return Expr;
         when Operator_Expr =>
            if Expr.Operator_Name = "&" then
               declare
                  Left : constant IDL_Expression :=
                           Evaluate_Static_Expression (Expr.Left);
                  Right : constant IDL_Expression :=
                            Evaluate_Static_Expression (Expr.Right);
               begin
                  if Is_String_Literal (Left)
                    and then Is_String_Literal (Right)
                  then
                     return new IDL_Expression_Record'
                       (String_Expr,
                        Left.String_Value & Right.String_Value);
                  else
                     return Expr;
                  end if;
               end;
            else
               return Expr;
            end if;
         when List_Expr =>
            return Expr;
      end case;
   end Evaluate_Static_Expression;

   function Get_Integer_Value (Expr : IDL_Expression) return Integer
   is (Expr.Integer_Value);

   function Get_Length (Expr : IDL_Expression) return Natural
   is (if Expr = null
       then 0
       elsif Expr.Expr = List_Expr
       then Expr.List_Value.Last_Index
       else 1);

   function Get_Object_Name (Expr : IDL_Expression) return String
   is (To_String (Expr.Object_Name));

   function Get_String_Value (Expr : IDL_Expression) return String
   is (To_String (Expr.String_Value));

   function Has_Aspect
     (Aspects : IDL_Aspects;
      Name    : String)
      return Boolean
   is (Aspect (Aspects, Name) /= null);

   function Is_Integer_Literal (Expr : IDL_Expression) return Boolean
   is (Expr.Expr = Integer_Expr);

   function Is_List (Expr : IDL_Expression) return Boolean
   is (Expr.Expr = List_Expr);

   function Is_Object_Name (Expr : IDL_Expression) return Boolean
   is (Expr.Expr = Object_Expr);

   function Is_String_Literal (Expr : IDL_Expression) return Boolean
   is (Expr.Expr = String_Expr);

   ----------------------------
   -- New_Integer_Expression --
   ----------------------------

   function New_Integer_Expression
     (Value : Integer)
      return IDL_Expression
   is
   begin
      return new IDL_Expression_Record'
        (Integer_Expr, Value);
   end New_Integer_Expression;

   ------------------------
   -- New_Invoke_Command --
   ------------------------

   function New_Invoke_Command
     (Cap_Name : String;
      Endpoint : Positive)
      return IDL_Command
   is
   begin
      return new IDL_Command_Record'
        (Invoke, new IDL_Aspects_Record,
         To_Unbounded_String (Cap_Name), Endpoint);
   end New_Invoke_Command;

   -------------------------
   -- New_List_Expression --
   -------------------------

   function New_List_Expression return IDL_Expression is
   begin
      return new IDL_Expression_Record'
        (List_Expr, List_Value => <>);
   end New_List_Expression;

   ---------------------------
   -- New_Object_Expression --
   ---------------------------

   function New_Object_Expression
     (Object_Name : String)
      return IDL_Expression
   is
   begin
      return new IDL_Expression_Record'
        (Object_Expr, To_Unbounded_String (Object_Name));
   end New_Object_Expression;

   -----------------------------
   -- New_Operator_Expression --
   -----------------------------

   function New_Operator_Expression
     (Operator    : String;
      Left, Right : IDL_Expression)
      return IDL_Expression
   is
   begin
      return new IDL_Expression_Record'
        (Operator_Expr, To_Unbounded_String (Operator),
         Left, Right);
   end New_Operator_Expression;

   -------------------------------
   -- New_Procedure_Declaration --
   -------------------------------

   function New_Procedure_Declaration
     (Name : String)
      return IDL_Procedure
   is
   begin
      if Table.Is_Empty then
         Table.Insert
           ("nl", New_String_Expression ((1 => Character'Val (10))));
      end if;

      return new IDL_Procedure_Record'
        (Declared_Name => To_Unbounded_String (Name),
         Standard_Name => To_Unbounded_String (To_Lower (Name)),
         Arguments     => <>,
         Locals        => <>,
         Commands      => <>);
   end New_Procedure_Declaration;

   ---------------------------
   -- New_String_Expression --
   ---------------------------

   function New_String_Expression
     (Literal : String)
      return IDL_Expression
   is
   begin
      return new IDL_Expression_Record'
        (String_Expr, To_Unbounded_String (Literal));
   end New_String_Expression;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Expression : IDL_Expression;
      Process    : not null access
        procedure (Expr : IDL_Expression))
   is
   begin
      if Expression = null then
         null;
      elsif Expression.Expr = List_Expr then
         for Expr of Expression.List_Value loop
            Process (Expr);
         end loop;
      else
         Process (Expression);
      end if;
   end Scan;

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (Proc    : IDL_Procedure;
      Process : not null access
        procedure (Arg_Name : String;
                   Arg_Type : IDL.Types.IDL_Type))
   is
   begin
      for Arg of Proc.Arguments loop
         Process (To_String (Arg.Declared_Name), Arg.Typ);
      end loop;
   end Scan_Arguments;

   ------------------
   -- Scan_Aspects --
   ------------------

   procedure Scan_Aspects
     (Aspects : IDL_Aspects;
      Process : not null access
        procedure (Name    : String;
                   Value   : IDL_Expression))
   is
   begin
      for Aspect of Aspects.Vector loop
         Process (To_Lower (To_String (Aspect.Name)), Aspect.Value);
      end loop;
   end Scan_Aspects;

   -------------------
   -- Scan_Commands --
   -------------------

   procedure Scan_Commands
     (Proc    : IDL_Procedure;
      Process_Invoke : not null access
        procedure (Cap_Name : String;
                   Endpoint : Positive;
                   Aspects  : IDL_Aspects))
   is
   begin
      for Command of Proc.Commands loop
         Process_Invoke
           (To_String (Command.Cap_Name),
            Command.Endpoint, Command.Aspects);
      end loop;
   end Scan_Commands;

   -----------------
   -- Scan_Locals --
   -----------------

   procedure Scan_Locals
     (Proc    : IDL_Procedure;
      Process : not null access
        procedure (Local_Name : String;
                   Local_Type : IDL.Types.IDL_Type))
   is
   begin
      for Local of Proc.Locals loop
         Process (To_String (Local.Declared_Name), Local.Typ);
      end loop;
   end Scan_Locals;

   -------------------
   -- Standard_Name --
   -------------------

   function Standard_Name (Proc : IDL_Procedure) return String is
   begin
      return To_String (Proc.Standard_Name);
   end Standard_Name;

end IDL.Procs;
