with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

with WL.String_Sets;

with IDL.Identifiers;

package body IDL.Syntax is

   Max_Arguments : constant := 8;

   type IDL_Argument_Record is
      record
         Arg_Name      : Unbounded_String;
         Arg_Mode      : IDL_Argument_Mode;
         Arg_Type      : IDL.Types.IDL_Type;
         Arg_Length    : Unbounded_String;
         Arg_Index     : IDL_Argument;
      end record;

   type IDL_Subprogram_Record is
      record
         From               : IDL_Interface;
         Name               : Unbounded_String;
         Is_Function        : Boolean;
         Num_Args           : Natural;
         Args               : IDL_Argument_Array (1 .. Max_Arguments);
         Result_Arg         : IDL_Argument;
         Return_Buffer_Size : Natural := 1024;
      end record;

   type Object_Record is
      record
         Name                 : Unbounded_String;
         Object_Type          : IDL.Types.IDL_Type;
         Object_Value         : Unbounded_String;
         Object_Is_Constraint : Boolean;
         Object_Is_Const      : Boolean;
         Object_Is_Type       : Boolean;
      end record;

   Null_Object : constant Object_Record :=
                   (Null_Unbounded_String, IDL.Types.No_Type,
                    Null_Unbounded_String, False, False, False);

   Max_Subprograms : constant := 24;
   Max_Context     : constant := 8;
   Max_Inherited   : constant := 8;
   Max_Objects     : constant := 100;

   subtype Interface_Subprogram_Array is
     IDL_Subprogram_Array (1 .. Max_Subprograms);

   type Context_Array is
     array (Positive range 1 .. Max_Context) of Unbounded_String;

   type Inheritance_Array is
     array (Positive range 1 .. Max_Inherited) of IDL_Interface;

   type Object_Array is
     array (Positive range 1 .. Max_Objects) of Object_Record;

   type IDL_Interface_Record is
      record
         Name          : Unbounded_String;
         Num_Contexts  : Natural;
         Contexts      : Context_Array;
         Num_Inherited : Natural;
         Inherited     : Inheritance_Array;
         Num_Subprs    : Natural;
         Subprs        : Interface_Subprogram_Array;
         Num_Objects   : Natural;
         Objects       : Object_Array;
         Identity      : Natural;
      end record;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument (Item     : IDL_Subprogram;
                           Name     : String;
                           Arg_Type : IDL.Types.IDL_Type;
                           Mode     : IDL_Argument_Mode := In_Argument)
   is
      use IDL.Types;
   begin
      Item.Num_Args := Item.Num_Args + 1;
      Item.Args (Item.Num_Args) :=
        new IDL_Argument_Record'
          (Arg_Name      => To_Unbounded_String (Name),
           Arg_Type      => Arg_Type,
           Arg_Mode      => Mode,
           others        => <>);
      if IDL.Types.Get_Ada_Package (Arg_Type) /= ""
        and then IDL.Types.Get_Name (Arg_Type) /= Get_Name (Item.From)
      then
         Add_Context (Item.From, IDL.Types.Get_Ada_Package (Arg_Type));
      end if;

      if False then
         if Has_Count_Type (Arg_Type) then
            declare
               Array_Arg : constant IDL_Argument :=
                             Item.Args (Item.Num_Args);
            begin
               Add_Argument (Item, Name & "_Last", Get_Count_Type (Arg_Type),
                             Out_Argument);
               Array_Arg.Arg_Index := Item.Args (Item.Num_Args);
            end;
         end if;
      end if;
   end Add_Argument;

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Aspect (Item         : IDL_Interface;
                         Aspect_Name  : String;
                         Value        : Natural)
   is
   begin
      if Aspect_Name = "identity" then
         Item.Identity := Value;
      else
         raise Constraint_Error with "no such aspect: " & Aspect_Name;
      end if;
   end Add_Aspect;

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Aspect (Item         : IDL_Subprogram;
                         Aspect_Name  : String;
                         Attribute    : String;
                         Value        : String)
   is
      function Equal (Left, Right : String) return Boolean
        renames Ada.Strings.Fixed.Equal_Case_Insensitive;
   begin
      if Attribute = "length" then
         for I in 1 .. Item.Num_Args loop
            if Equal (Get_Name (Item.Args (I)), Aspect_Name) then
               Item.Args (I).Arg_Length := To_Unbounded_String (Value);
               return;
            end if;
         end loop;
         raise Constraint_Error with "no such argument: " & Aspect_Name
           & " for attribute " & Attribute;
      end if;

      raise Constraint_Error with "no such aspect: " & Aspect_Name
        & (if Attribute = "" then ""
           else " with attribute " & Attribute);
   end Add_Aspect;

   ------------------
   -- Add_Constant --
   ------------------

   procedure Add_Constant (Item      : IDL_Interface;
                           Name      : String;
                           Data_Type : IDL.Types.IDL_Type;
                           Value     : String)
   is
   begin
      Item.Num_Objects := Item.Num_Objects + 1;
      Item.Objects (Item.Num_Objects) :=
        (Name                 => To_Unbounded_String (Name),
         Object_Type          => Data_Type,
         Object_Value         => To_Unbounded_String (Value),
         Object_Is_Constraint => False,
         Object_Is_Const      => True,
         Object_Is_Type       => False);
   end Add_Constant;

   --------------------
   -- Add_Constraint --
   --------------------

   procedure Add_Constraint
     (Item            : IDL_Interface;
      Constraint_Name : String;
      Constraint_Type : IDL.Types.IDL_Type)
   is
      Constraint_Subpr : constant IDL_Subprogram :=
                           New_Subprogram (Item, Constraint_Name);
   begin
      Set_Result_Type (Constraint_Subpr, Constraint_Type);
      Item.Num_Objects := Item.Num_Objects + 1;
      Item.Objects (Item.Num_Objects) :=
        (Name                 => To_Unbounded_String (Constraint_Name),
         Object_Type          => Constraint_Type,
         Object_Value         => Null_Unbounded_String,
         Object_Is_Constraint => True,
         Object_Is_Const      => False,
         Object_Is_Type       => False);
   end Add_Constraint;

   -----------------
   -- Add_Context --
   -----------------

   procedure Add_Context (Item         : IDL_Interface;
                          Package_Name : String)
   is
   begin
      if Get_Package_Name (Item) = Package_Name then
         return;
      end if;
      for I in 1 .. Item.Num_Contexts loop
         if Item.Contexts (I) = Package_Name then
            return;
         end if;
      end loop;

      Item.Num_Contexts := Item.Num_Contexts + 1;
      Item.Contexts (Item.Num_Contexts) :=
        To_Unbounded_String (Package_Name);
   end Add_Context;

   ----------------------
   -- Add_Derived_Type --
   ----------------------

   procedure Add_Derived_Type (Item      : IDL_Interface;
                               Name      : String;
                               Data_Type : IDL.Types.IDL_Type)
   is
   begin
      Item.Num_Objects := Item.Num_Objects + 1;
      Item.Objects (Item.Num_Objects) :=
        (Name         => To_Unbounded_String (Name),
         Object_Type  => Data_Type,
         Object_Value => Null_Unbounded_String,
         Object_Is_Constraint => False,
         Object_Is_Const      => False,
         Object_Is_Type       => True);
      IDL.Types.Add_Derived_Type (Name, Data_Type);
   end Add_Derived_Type;

   -------------------
   -- Add_Inherited --
   -------------------

   procedure Add_Inherited (To     : IDL_Interface;
                            Parent : IDL_Interface)
   is
   begin
      To.Num_Inherited := To.Num_Inherited + 1;
      To.Inherited (To.Num_Inherited) := Parent;
--        Add_Context (To, "Rose.Interfaces." & Get_Name (Parent));
--        for I in 1 .. Get_Num_Contexts (Parent) loop
--           Add_Context (To, Get_Context (Parent, I));
--        end loop;
   end Add_Inherited;

   --------------
   -- Add_Type --
   --------------

   procedure Add_Type (Item      : IDL_Interface;
                       Name      : String;
                       Item_Type : IDL.Types.IDL_Type)
   is
   begin
      Item.Num_Objects := Item.Num_Objects + 1;
      Item.Objects (Item.Num_Objects) :=
        (Name         => To_Unbounded_String (Name),
         Object_Type  => Item_Type,
         Object_Value => Null_Unbounded_String,
         Object_Is_Constraint => False,
         Object_Is_Const      => False,
         Object_Is_Type       => True);
      IDL.Types.Add_Named_Type (Name, Item_Type);
   end Add_Type;

   ---------------------------
   -- Full_Subprogram_Count --
   ---------------------------

   function Full_Subprogram_Count
     (Item : IDL_Interface)
      return Natural
   is
      Result : Natural := 0;

      procedure Add_To_Count (Subpr : IDL_Subprogram);

      ------------------
      -- Add_To_Count --
      ------------------

      procedure Add_To_Count (Subpr : IDL_Subprogram) is
         pragma Unreferenced (Subpr);
      begin
         Result := Result + 1;
      end Add_To_Count;

   begin
      Scan_Subprograms (Item, True, Add_To_Count'Access);
      return Result;
   end Full_Subprogram_Count;

   ------------------
   -- Get_Ada_Name --
   ------------------

   function Get_Ada_Name (Item : IDL_Interface) return String is
   begin
      return IDL.Identifiers.To_Ada_Name (Get_Name (Item));
   end Get_Ada_Name;

   ------------------
   -- Get_Ada_Name --
   ------------------

   function Get_Ada_Name (Item : IDL_Argument) return String is
   begin
      return IDL.Identifiers.To_Ada_Name (Get_Name (Item));
   end Get_Ada_Name;

   ------------------
   -- Get_Ada_Name --
   ------------------

   function Get_Ada_Name (Item : IDL_Subprogram) return String is
   begin
      return IDL.Identifiers.To_Ada_Name (Get_Name (Item));
   end Get_Ada_Name;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments
     (Item : IDL_Subprogram)
      return IDL_Argument_Array
   is
   begin
      return Item.Args (1 .. Item.Num_Args);
   end Get_Arguments;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (Item  : IDL_Interface;
                         Index : Positive)
                         return String
   is
   begin
      return To_String (Item.Contexts (Index));
   end Get_Context;

   ------------------
   -- Get_Identity --
   ------------------

   function Get_Identity (Item : IDL_Interface) return Natural is
   begin
      return Item.Identity;
   end Get_Identity;

   -------------------
   -- Get_Inherited --
   -------------------

   function Get_Inherited (Item  : IDL_Interface;
                           Index : Positive)
                           return IDL_Interface
   is
   begin
      return Item.Inherited (Index);
   end Get_Inherited;

   -----------------------------
   -- Get_Last_Index_Argument --
   -----------------------------

   function Get_Last_Index_Argument
     (Item : IDL_Argument)
      return IDL_Argument
   is (Item.Arg_Index);

   ---------------------------
   -- Get_Length_Constraint --
   ---------------------------

   function Get_Length_Constraint
     (Item        : IDL_Argument;
      Object_Name : String)
      return String
   is
   begin
      if Item.Arg_Length = Null_Unbounded_String then
         return "4096";
      else
         return Object_Name & "." & To_String (Item.Arg_Length);
      end if;
   end Get_Length_Constraint;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode (Item : IDL_Argument) return IDL_Argument_Mode is
   begin
      return Item.Arg_Mode;
   end Get_Mode;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode (Item : IDL_Argument) return String is
   begin
      case Get_Mode (Item) is
         when In_Argument =>
            return "in";
         when Out_Argument =>
            return "out";
         when Inout_Argument =>
            return "in out";
      end case;
   end Get_Mode;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : IDL_Interface) return String is
   begin
      return To_String (Item.Name);
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : IDL_Subprogram) return String is
   begin
      return To_String (Item.Name);
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : IDL_Argument) return String is
   begin
      return To_String (Item.Arg_Name);
   end Get_Name;

   ----------------------
   -- Get_Num_Contexts --
   ----------------------

   function Get_Num_Contexts (Item : IDL_Interface) return Natural is
   begin
      return Item.Num_Contexts;
   end Get_Num_Contexts;

   -----------------------
   -- Get_Num_Inherited --
   -----------------------

   function Get_Num_Inherited (Item : IDL_Interface) return Natural is
   begin
      return Item.Num_Inherited;
   end Get_Num_Inherited;

   ---------------------
   -- Get_Num_Objects --
   ---------------------

   function Get_Num_Objects (Item : IDL_Interface) return Natural is
   begin
      return Item.Num_Objects;
   end Get_Num_Objects;

   -------------------------
   -- Get_Object_Ada_Name --
   -------------------------

   function Get_Object_Ada_Name
     (Item  : IDL_Interface;
      Index : Positive)
      return String is
   begin
      return IDL.Identifiers.To_Ada_Name
        (To_String (Item.Objects (Index).Name));
   end Get_Object_Ada_Name;

   ----------------------------
   -- Get_Object_Is_Constant --
   ----------------------------

   function Get_Object_Is_Constant
     (Item  : IDL_Interface;
      Index : Positive)
      return Boolean is
   begin
      return Item.Objects (Index).Object_Is_Const;
   end Get_Object_Is_Constant;

   function Get_Object_Is_Constraint
     (Item    : IDL_Interface;
      Index   : Positive)
      return Boolean
   is (Item.Objects (Index).Object_Is_Constraint);

   ------------------------
   -- Get_Object_Is_Type --
   ------------------------

   function Get_Object_Is_Type
     (Item  : IDL_Interface;
      Index : Positive)
      return Boolean is
   begin
      return Item.Objects (Index).Object_Is_Type;
   end Get_Object_Is_Type;

   ---------------------
   -- Get_Object_Type --
   ---------------------

   function Get_Object_Type
     (Item  : IDL_Interface;
      Index : Positive)
      return IDL.Types.IDL_Type is
   begin
      return Item.Objects (Index).Object_Type;
   end Get_Object_Type;

   ----------------------
   -- Get_Object_Value --
   ----------------------

   function Get_Object_Value
     (Item  : IDL_Interface;
      Index : Positive)
      return String is
   begin
      return To_String (Item.Objects (Index).Object_Value);
   end Get_Object_Value;

   ----------------------
   -- Get_Package_Name --
   ----------------------

   function Get_Package_Name (Item : IDL_Interface) return String is
   begin
      return "Rose.Interfaces." & Get_Ada_Name (Item);
   end Get_Package_Name;

   ------------------------
   -- Get_Qualified_Name --
   ------------------------

   function Get_Qualified_Name (Item : IDL_Interface) return String is
   begin
      return Get_Package_Name (Item) & "." & Get_Ada_Name (Item) &
      "_Interface";
   end Get_Qualified_Name;

   -----------------------------
   -- Get_Qualified_Root_Name --
   -----------------------------

   function Get_Qualified_Root_Name (Item : IDL_Interface) return String is
   begin
      return Get_Package_Name (Item) & "." & Get_Root_Name (Item);
   end Get_Qualified_Root_Name;

   -------------------------
   -- Get_Return_Argument --
   -------------------------

   function Get_Return_Argument
     (Item : IDL_Subprogram)
      return IDL_Argument
   is
   begin
      return Item.Result_Arg;
   end Get_Return_Argument;

   ----------------------------
   -- Get_Return_Buffer_Size --
   ----------------------------

   function Get_Return_Buffer_Size
     (Item : IDL_Subprogram)
      return Natural
   is
   begin
      return Item.Return_Buffer_Size;
   end Get_Return_Buffer_Size;

   ---------------------
   -- Get_Return_Type --
   ---------------------

   function Get_Return_Type (Item : IDL_Subprogram)
                             return IDL.Types.IDL_Type
   is
   begin
      return Get_Type (Item.Result_Arg);
   end Get_Return_Type;

   -------------------
   -- Get_Root_Name --
   -------------------

   function Get_Root_Name (Item : IDL_Interface) return String is
   begin
      return
        "Root_" & IDL.Identifiers.To_Ada_Name (Get_Name (Item)) &
      "_Interface";
   end Get_Root_Name;

   ---------------------
   -- Get_Subprograms --
   ---------------------

   function Get_Subprograms
     (Item : IDL_Interface)
      return IDL_Subprogram_Array
   is
   begin
      return Item.Subprs (1 .. Item.Num_Subprs);
   end Get_Subprograms;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Item : IDL_Argument) return IDL.Types.IDL_Type is
   begin
      return Item.Arg_Type;
   end Get_Type;

   ------------------
   -- Has_Ancestor --
   ------------------

   function Has_Ancestor (Child    : IDL_Interface;
                          Ancestor : IDL_Interface)
                          return Boolean
   is
   begin
      for I in 1 .. Get_Num_Inherited (Child) loop
         if Get_Inherited (Child, I) = Ancestor or else
           Has_Ancestor (Get_Inherited (Child, I), Ancestor)
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Ancestor;

   -----------------------------
   -- Has_Last_Index_Argument --
   -----------------------------

   function Has_Last_Index_Argument (Item : IDL_Argument) return Boolean
   is (Item.Arg_Index /= null);

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function (Item : IDL_Subprogram) return Boolean is
   begin
      return Item.Is_Function;
   end Is_Function;

   -------------------
   --  Is_Procedure --
   -------------------

   function Is_Procedure (Item : IDL_Subprogram) return Boolean is
   begin
      return not Item.Is_Function;
   end Is_Procedure;

   -------------------
   -- New_Interface --
   -------------------

   function New_Interface
     (Name     : String)
      return IDL_Interface
   is
      Result : constant IDL_Interface :=
                 new IDL_Interface_Record'
                   (Name          => To_Unbounded_String (Name),
                    Num_Contexts  => 0,
                    Contexts      => (others => Null_Unbounded_String),
                    Num_Inherited => 0,
                    Inherited     => (others => null),
                    Num_Subprs    => 0,
                    Subprs        => (others => null),
                    Num_Objects   => 0,
                    Objects       => (others => Null_Object),
                    Identity      => 0);
   begin
      return Result;
   end New_Interface;

   --------------------
   -- New_Subprogram --
   --------------------

   function New_Subprogram
     (Item   : IDL_Interface;
      Name   : String)
      return IDL_Subprogram
   is
   begin
      New_Subprogram (Item, Name);
      return Item.Subprs (Item.Num_Subprs);
   end New_Subprogram;

   --------------------
   -- New_Subprogram --
   --------------------

   procedure New_Subprogram
     (Item   : IDL_Interface;
      Name   : String)
   is
   begin
      Item.Num_Subprs := Item.Num_Subprs + 1;
      Item.Subprs (Item.Num_Subprs) :=
        new IDL_Subprogram_Record'
          (From               => Item,
           Name               => To_Unbounded_String (Name),
           Is_Function        => False,
           Num_Args           => 0,
           Args               => (others => null),
           Result_Arg         => null,
           Return_Buffer_Size => <>);
   end New_Subprogram;

   procedure Scan_Ancestors
     (Item            : IDL_Interface;
      Include_Current : Boolean;
      Process         : not null access
        procedure (Ancestor : IDL_Interface))
   is
      Set : WL.String_Sets.Set;

      procedure Scan (Ancestor : IDL_Interface);

      ----------
      -- Scan --
      ----------

      procedure Scan (Ancestor : IDL_Interface) is
      begin
         Set.Insert (Get_Ada_Name (Ancestor));
         for I in 1 .. Get_Num_Inherited (Ancestor) loop
            declare
               Next : constant IDL_Interface := Get_Inherited (Ancestor, I);
            begin
               if not Set.Contains (Get_Ada_Name (Next)) then
                  Scan (Next);
                  Process (Next);
               end if;
            end;
         end loop;
      end Scan;

   begin
      Scan (Item);
      if Include_Current then
         Process (Item);
      end if;
   end Scan_Ancestors;

   ----------------------
   -- Scan_Constraints --
   ----------------------

   procedure Scan_Constraints
     (Item              : IDL_Interface;
      Include_Inherited : Boolean;
      Process           : not null access
        procedure (Name  : String;
                   Constraint_Type : IDL.Types.IDL_Type))
   is
      pragma Unreferenced (Include_Inherited);
   begin
      for I in 1 .. Item.Num_Objects loop
         if Item.Objects (I).Object_Is_Constraint then
            Process
              (IDL.Identifiers.To_Ada_Name (To_String (Item.Objects (I).Name)),
               Item.Objects (I).Object_Type);
         end if;
      end loop;
   end Scan_Constraints;

   ----------------------
   -- Scan_Subprograms --
   ----------------------

   procedure Scan_Subprograms
     (Item              : IDL_Interface;
      Include_Inherited : Boolean;
      Process           : not null access
        procedure (Item : IDL_Subprogram))
   is
      procedure Scan (X : IDL_Interface);

      ----------
      -- Scan --
      ----------

      procedure Scan (X : IDL_Interface) is
      begin
         for I in 1 .. X.Num_Subprs loop
            Process (X.Subprs (I));
         end loop;
         if Include_Inherited then
            for J in 1 .. X.Num_Inherited loop
               Scan (X.Inherited (J));
            end loop;
         end if;
      end Scan;

   begin
      Scan (Item);
   end Scan_Subprograms;

   ---------------------
   -- Set_Result_Type --
   ---------------------

   procedure Set_Result_Type (Item        : IDL_Subprogram;
                              Result_Type : IDL.Types.IDL_Type)
   is
   begin
      Item.Is_Function := True;
      Item.Result_Arg :=
        new IDL_Argument_Record'
          (Arg_Name   => To_Unbounded_String ("Result"),
           Arg_Mode   => Out_Argument,
           Arg_Type   => Result_Type,
           others     => <>);
      if IDL.Types.Get_Ada_Package (Result_Type) /= "" then
         Add_Context (Item.From, IDL.Types.Get_Ada_Package (Result_Type));
      end if;
   end Set_Result_Type;

end IDL.Syntax;
