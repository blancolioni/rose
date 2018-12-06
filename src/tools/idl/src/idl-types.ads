package IDL.Types is

   No_Such_Type : exception;

   type IDL_Type is private;

   No_Type : constant IDL_Type;

   function Get_Name (Item : IDL_Type) return String;
   function Get_Ada_Package (Item : IDL_Type) return String;
   function Get_Ada_Name (Item : IDL_Type) return String;

   function Get_Type (Name : String) return IDL_Type;

   function Get_Size (Item : IDL_Type) return Natural;

   function Is_Capability (Item : IDL_Type) return Boolean;
   function Is_Capability_Array (Item : IDL_Type) return Boolean;
   function Is_Address (Item : IDL_Type) return Boolean;
   function Is_Scalar (Item : IDL_Type) return Boolean;
   function Is_Interface (Item : IDL_Type) return Boolean;
   function Is_String (Item : IDL_Type) return Boolean;
   function Is_Stream (Item : IDL_Type) return Boolean;
   function Is_Integer_Type (Item : IDL_Type) return Boolean;
   function Is_Word_Type (Item : IDL_Type) return Boolean;
   function Is_Word_32 (Item : IDL_Type) return Boolean;
   function Is_Word_64 (Item : IDL_Type) return Boolean;

   function Has_Count_Type (Item : IDL_Type) return Boolean;
   function Get_Count_Type (Item : IDL_Type) return IDL_Type;

   procedure Add_Type (Name     : String;
                       Ada_Name : String);

   procedure Add_Named_Type (Name      : String;
                             Item_Type : IDL_Type);

   procedure Add_Derived_Type (Name    : String;
                               Parent  : IDL_Type);

   function New_Range_Type
     (Interface_Name : String;
      Low, High      : Integer)
      return IDL_Type;

   function New_Enumerated_Type
     (Interface_Name : String)
      return IDL_Type;

   procedure Add_Literal
     (To_Enum  : IDL_Type;
      Name     : String);

   function Is_Range_Type (Item : IDL_Type) return Boolean;
   function Get_Low (Item : IDL_Type) return Natural
     with Pre => Is_Range_Type (Item);
   function Get_High (Item : IDL_Type) return Natural
     with Pre => Is_Range_Type (Item);

   function Is_Enumerated_Type (Item : IDL_Type) return Boolean;
   function Get_Literal_Count (Item : IDL_Type) return Natural
     with Pre => Is_Enumerated_Type (Item);

   function Get_Literal_Name
     (Item  : IDL_Type;
      Index : Positive)
      return String
     with Pre => Is_Enumerated_Type (Item);

   function New_Record_Type return IDL_Type;
   procedure Add_Field (To_Record  : IDL_Type;
                        Name       : String;
                        Field_Type : IDL_Type);
   function Is_Record_Type (Item : IDL_Type) return Boolean;
   function Get_Field_Count (Item : IDL_Type) return Natural;
   function Get_Field_Name (Item  : IDL_Type;
                            Index : Positive)
                            return String;

   function Get_Field_Type (Item  : IDL_Type;
                            Index : Positive)
                            return IDL_Type;

   procedure Add_Type
     (Name        : String;
      Ada_Package : String;
      Ada_Name    : String;
      Size        : Natural := 0);

   procedure Add_Interface_Type
     (Name        : String);

   procedure Create_Standard_Types;

private

   type IDL_Type_Record;
   type IDL_Type is access IDL_Type_Record;

   No_Type : constant IDL_Type := null;

   function Is_Integer_Type (Item : IDL_Type) return Boolean
   is (Get_Ada_Name (Item) = "Integer"
       or else Get_Ada_Name (Item) = "Natural"
       or else Get_Ada_Name (Item) = "Positive");

   function Is_Word_32 (Item : IDL_Type) return Boolean
   is (Get_Ada_Name (Item) = "Rose.Words.Word_32");

   function Is_Word_64 (Item : IDL_Type) return Boolean
   is (Get_Ada_Name (Item) = "Rose.Words.Word_64");

   function Is_Word_Type (Item : IDL_Type) return Boolean
   is (Is_Word_32 (Item) or else Is_Word_64 (Item));

   function Is_Capability_Array (Item : IDL_Type) return Boolean
   is (Get_Ada_Name (Item) = "Rose.Capabilities.Capability_Array");

end IDL.Types;
