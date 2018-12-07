with IDL.Types;

package IDL.Syntax is

   type IDL_Interface is private;

   Null_Interface : constant IDL_Interface;

   type IDL_Subprogram is private;

   type IDL_Subprogram_Array is array (Positive range <>) of IDL_Subprogram;

   type IDL_Argument is private;
   type IDL_Argument_Array is array (Positive range <>) of IDL_Argument;

   type IDL_Argument_Mode is (In_Argument, Out_Argument, Inout_Argument);

   function Get_Name (Item : IDL_Interface) return String;
   function Get_Ada_Name (Item : IDL_Interface) return String;
   function Get_Root_Name (Item : IDL_Interface) return String;
   function Get_Package_Name (Item : IDL_Interface) return String;
   function Get_Qualified_Name (Item : IDL_Interface) return String;
   function Get_Qualified_Root_Name (Item : IDL_Interface) return String;
   function Get_Identity (Item : IDL_Interface) return Natural;

   function Get_Num_Contexts (Item : IDL_Interface) return Natural;
   function Get_Context (Item   : IDL_Interface;
                         Index  : Positive)
                         return String;

   function Get_Num_Inherited (Item : IDL_Interface) return Natural;
   function Get_Inherited (Item   : IDL_Interface;
                           Index  : Positive)
                           return IDL_Interface;

   function Has_Ancestor (Child    : IDL_Interface;
                          Ancestor : IDL_Interface)
                          return Boolean;

   procedure Scan_Ancestors
     (Item            : IDL_Interface;
      Include_Current : Boolean;
      Process         : not null access
        procedure (Ancestor : IDL_Interface));

   function Get_Num_Objects (Item : IDL_Interface)
                             return Natural;
   function Get_Object_Ada_Name (Item    : IDL_Interface;
                                 Index   : Positive)
                                 return String;

   function Get_Object_Type (Item    : IDL_Interface;
                             Index   : Positive)
                             return IDL.Types.IDL_Type;

   function Get_Object_Value (Item    : IDL_Interface;
                              Index   : Positive)
                              return String;

   function Get_Object_Is_Constraint (Item    : IDL_Interface;
                                      Index   : Positive)
                                      return Boolean;

   function Get_Object_Is_Constant (Item    : IDL_Interface;
                                    Index   : Positive)
                                    return Boolean;

   function Get_Object_Is_Type (Item  : IDL_Interface;
                                Index : Positive)
                                return Boolean;

   procedure Scan_Constraints
     (Item              : IDL_Interface;
      Include_Inherited : Boolean;
      Process           : not null access
        procedure (Name  : String;
                   Constraint_Type : IDL.Types.IDL_Type));

   function Get_Subprograms
     (Item : IDL_Interface)
      return IDL_Subprogram_Array;

   function Full_Subprogram_Count
     (Item : IDL_Interface)
      return Natural;

   procedure Scan_Subprograms
     (Item              : IDL_Interface;
      Include_Inherited : Boolean;
      Process           : not null access
        procedure (Item : IDL_Subprogram));

   function Get_Name (Item : IDL_Subprogram) return String;
   function Get_Ada_Name (Item : IDL_Subprogram) return String;
   function Get_Arguments (Item : IDL_Subprogram) return IDL_Argument_Array;
   function Is_Function (Item : IDL_Subprogram) return Boolean;
   function Is_Procedure (Item : IDL_Subprogram) return Boolean;
   function Has_Scalar_Result (Item : IDL_Subprogram) return Boolean;

   function Get_Return_Type (Item : IDL_Subprogram)
                             return IDL.Types.IDL_Type;
   function Get_Return_Argument
     (Item : IDL_Subprogram)
      return IDL_Argument;

   function Get_Return_Buffer_Size
     (Item : IDL_Subprogram)
      return Natural;

   function Get_Name (Item : IDL_Argument) return String;
   function Get_Ada_Name (Item : IDL_Argument) return String;
   function Get_Type (Item : IDL_Argument)
                      return IDL.Types.IDL_Type;
   function Has_Last_Index_Argument (Item : IDL_Argument) return Boolean;
   function Get_Last_Index_Argument (Item : IDL_Argument) return IDL_Argument;

   function Get_Length_Constraint
     (Item        : IDL_Argument;
      Object_Name : String)
      return String;

   function Get_Mode (Item : IDL_Argument) return IDL_Argument_Mode;
   function Get_Mode (Item : IDL_Argument) return String;

   function New_Interface (Name     : String)
                           return IDL_Interface;

   procedure Add_Inherited (To     : IDL_Interface;
                            Parent : IDL_Interface);

   procedure Add_Context (Item         : IDL_Interface;
                          Package_Name : String);

   procedure Add_Aspect (Item         : IDL_Interface;
                         Aspect_Name  : String;
                         Value        : Natural);

   procedure Add_Constraint
     (Item            : IDL_Interface;
      Constraint_Name : String;
      Constraint_Type : IDL.Types.IDL_Type);

   procedure Add_Constant (Item      : IDL_Interface;
                           Name      : String;
                           Data_Type : IDL.Types.IDL_Type;
                           Value     : String);

   procedure Add_Derived_Type (Item      : IDL_Interface;
                               Name      : String;
                               Data_Type : IDL.Types.IDL_Type);

   procedure Add_Type (Item      : IDL_Interface;
                       Name      : String;
                       Item_Type : IDL.Types.IDL_Type);

   function New_Subprogram (Item   : IDL_Interface;
                            Name   : String)
                            return IDL_Subprogram;

   procedure New_Subprogram (Item   : IDL_Interface;
                             Name   : String);

   procedure Add_Argument (Item     : IDL_Subprogram;
                           Name     : String;
                           Arg_Type : IDL.Types.IDL_Type;
                           Mode     : IDL_Argument_Mode := In_Argument);

   procedure Add_Aspect (Item         : IDL_Subprogram;
                         Aspect_Name  : String;
                         Attribute    : String;
                         Value        : String);

   procedure Set_Result_Type (Item        : IDL_Subprogram;
                              Result_Type : IDL.Types.IDL_Type);

private

   type IDL_Interface_Record;
   type IDL_Interface is access IDL_Interface_Record;

   type IDL_Subprogram_Record;
   type IDL_Subprogram is access IDL_Subprogram_Record;

   type IDL_Argument_Record;
   type IDL_Argument is access IDL_Argument_Record;

   Null_Interface : constant IDL_Interface := null;

   function Has_Scalar_Result (Item : IDL_Subprogram) return Boolean
   is (Is_Function (Item)
       and then IDL.Types.Is_Scalar (Get_Return_Type (Item)));

end IDL.Syntax;
