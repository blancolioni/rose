with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with IDL.Identifiers;
with IDL.Options;

package body IDL.Types is

   function "+" (X : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Record_Field_Type is
      record
         Field_Name : Ada.Strings.Unbounded.Unbounded_String;
         Field_Type : IDL_Type;
      end record;

   package Record_Field_Vectors is
      new Ada.Containers.Vectors (Positive, Record_Field_Type);

   type IDL_Type_Record is
      record
         Name           : Ada.Strings.Unbounded.Unbounded_String;
         Ada_Package    : Ada.Strings.Unbounded.Unbounded_String;
         Ada_Name       : Ada.Strings.Unbounded.Unbounded_String;
         Fields         : Record_Field_Vectors.Vector;
         Derived_From   : IDL_Type      := null;
         Count_Type     : IDL_Type      := null;
         Size           : Natural       := 0;
         Low, High      : Integer       := 0;
         Is_Range       : Boolean       := False;
         Is_Enumeration : Boolean       := False;
         Is_Record      : Boolean       := False;
         Is_Interface   : Boolean       := False;
         Is_Capability  : Boolean       := False;
         Is_Address     : Boolean       := False;
         Is_Internal    : Boolean       := False;
         Is_Scalar      : Boolean       := False;
      end record;

   package Type_Tables is
     new Ada.Containers.Hashed_Maps (Ada.Strings.Unbounded.Unbounded_String,
                                     IDL_Type,
                                     Ada.Strings.Unbounded.Hash,
                                     Ada.Strings.Unbounded."=");

   Type_Table : Type_Tables.Map;

   procedure Internal_Add_Type
     (Name          : String;
      Ada_Package   : String;
      Ada_Name      : String;
      Size          : Natural  := 0;
      Derived_From  : IDL_Type := null;
      Count_Type    : IDL_Type := null;
      Low           : Integer := 0;
      High          : Integer := 0;
      Is_Record     : Boolean := False;
      Is_Interface  : Boolean := False;
      Is_Capability : Boolean := False;
      Is_Address    : Boolean := False;
      Is_Internal   : Boolean := False;
      Is_Range      : Boolean := False;
      Is_Scalar     : Boolean := True);

   function Default_Size return Natural
   is (if IDL.Options.Generate_32_Bit
       then 32 else 64);

   ----------------------
   -- Add_Derived_Type --
   ----------------------

   procedure Add_Derived_Type (Name    : String;
                               Parent  : IDL.Types.IDL_Type)
   is
   begin
      Internal_Add_Type (Name          => Name,
                         Ada_Package   => "",
                         Ada_Name      => IDL.Identifiers.To_Ada_Name (Name),
                         Size          => Parent.Size,
                         Derived_From  => Parent,
                         Is_Interface  => Parent.Is_Interface,
                         Is_Capability => Parent.Is_Capability,
                         Is_Address    => Parent.Is_Address,
                         Is_Scalar     => Parent.Is_Scalar,
                         Is_Record     => Parent.Is_Record);
   end Add_Derived_Type;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field (To_Record  : IDL_Type;
                        Name       : String;
                        Field_Type : IDL_Type)
   is
   begin
      To_Record.Fields.Append
        ((Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Field_Type));
   end Add_Field;

   ------------------------
   -- Add_Interface_Type --
   ------------------------

   procedure Add_Interface_Type
     (Name        : String)
   is
   begin
      Internal_Add_Type
        (Name, "Rose.Interfaces." & Name & ".Client",
         Name & "_Client",
         Is_Interface => True);
   end Add_Interface_Type;

   -----------------
   -- Add_Literal --
   -----------------

   procedure Add_Literal
     (To_Enum  : IDL_Type;
      Name     : String)
   is

   begin
      To_Enum.Fields.Append ((+Name, null));
   end Add_Literal;

   --------------------
   -- Add_Named_Type --
   --------------------

   procedure Add_Named_Type (Name      : String;
                             Item_Type : IDL.Types.IDL_Type)
   is
      use Ada.Strings.Unbounded;
   begin
      Item_Type.Name := To_Unbounded_String (Name);
      Item_Type.Ada_Name :=
        To_Unbounded_String (IDL.Identifiers.To_Ada_Name (Name));
      Type_Table.Insert (Key      => To_Unbounded_String (Name),
                         New_Item => Item_Type);
   end Add_Named_Type;

   --------------
   -- Add_Type --
   --------------

   procedure Add_Type
     (Name        : String;
      Ada_Name    : String)
   is
   begin
      Add_Type (Name        => Name,
                Ada_Package => "",
                Ada_Name    => Ada_Name,
                Size        => Default_Size);
   end Add_Type;

   --------------
   -- Add_Type --
   --------------

   procedure Add_Type
     (Name        : String;
      Ada_Package : String;
      Ada_Name    : String;
      Size        : Natural := 0)
   is
   begin
      Internal_Add_Type (Name, Ada_Package, Ada_Name, Size => Size);
   end Add_Type;

   ---------------------------
   -- Create_Standard_Types --
   ---------------------------

   procedure Create_Standard_Types is
   begin
      Add_Type ("word", "Rose.Words", "Word", Default_Size);
      Add_Type ("word_32", "Rose.Words", "Word_32", 32);
      Add_Type ("word_64", "Rose.Words", "Word_64", 64);
      Add_Type ("endpoint", "Rose.Objects", "Endpoint_Id", 64);
      Add_Type ("object_id", "Rose.Objects", "Object_Id", 64);
      Add_Type ("natural", "", "Natural", 32);
      Add_Type ("positive", "", "Positive", 32);
      Add_Type ("integer", "", "Integer", 32);
      Internal_Add_Type
        ("rose_interface", "Rose.Interfaces", "Rose_Interface",
         Is_Interface => True);
      Internal_Add_Type ("address", "System", "Address",
                         Is_Address => True);
      Add_Type ("storage_count", "System.Storage_Elements", "Storage_Count");

      if IDL.Options.Disable_Stream_Elements then
         Add_Type ("stream_element_count",
                   "System.Storage_Elements", "Storage_Count");
         Add_Type ("stream_element_offset",
                   "System.Storage_Elements", "Storage_Offset");
      else
         Add_Type ("stream_element_count",
                   "Ada.Streams", "Stream_Element_Count");
         Add_Type ("stream_element_offset",
                   "Ada.Streams", "Stream_Element_Offset");
      end if;

      Internal_Add_Type ("storage_array", "System.Storage_Elements",
                         "Storage_Array",
                         Count_Type => Get_Type ("storage_offset"),
                         Is_Scalar => False);

      if IDL.Options.Disable_Stream_Elements then
         Internal_Add_Type
           ("stream_element_array", "System.Storage_Elements",
            "Storage_Array",
            Count_Type => Get_Type ("storage_offset"),
            Is_Scalar  => False);
      else
         Internal_Add_Type ("stream_element_array",
                            "Ada.Streams",
                            "Stream_Element_Array",
                            Count_Type => Get_Type ("stream_element_offset"),
                            Is_Scalar  => False);
      end if;

      Internal_Add_Type ("page", "System.Storage_Elements",
                         "Storage_Array (1 .. 4096) with Alignment => 4096",
                         Is_Scalar => False, Is_Internal => True);
      Internal_Add_Type ("capability_array", "Rose.Capabilities",
                         "Capability_Array",
                         Is_Scalar => False);
      Internal_Add_Type ("capability", "Rose.Capabilities", "Capability",
                         Is_Capability    => True);
      Internal_Add_Type
        ("string", "", "String",
         Is_Scalar => False,
         Count_Type => Get_Type ("natural"));
   end Create_Standard_Types;

   ------------------
   -- Get_Ada_Name --
   ------------------

   function Get_Ada_Name (Item : IDL_Type) return String is
      use Ada.Strings.Unbounded;
      use IDL.Identifiers;
   begin
      if Item.Ada_Package = Null_Unbounded_String then
         return To_Ada_Name (To_String (Item.Ada_Name));
      else
         return To_Ada_Name
           (To_String (Item.Ada_Package & '.' & Item.Ada_Name));
      end if;
   end Get_Ada_Name;

   ---------------------
   -- Get_Ada_Package --
   ---------------------

   function Get_Ada_Package (Item : IDL_Type) return String is
      use Ada.Strings.Unbounded;
      use IDL.Identifiers;
   begin
      if Item.Ada_Package = Null_Unbounded_String then
         return "";
      else
         return To_Ada_Name (To_String (Item.Ada_Package));
      end if;
   end Get_Ada_Package;

   --------------------
   -- Get_Count_Type --
   --------------------

   function Get_Count_Type (Item : IDL_Type) return IDL_Type is
   begin
      return Item.Count_Type;
   end Get_Count_Type;

   ---------------------
   -- Get_Field_Count --
   ---------------------

   function Get_Field_Count (Item : IDL_Type) return Natural is
   begin
      return Item.Fields.Last_Index;
   end Get_Field_Count;

   --------------------
   -- Get_Field_Name --
   --------------------

   function Get_Field_Name (Item  : IDL_Type;
                            Index : Positive)
                            return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Item.Fields.Element (Index).Field_Name);
   end Get_Field_Name;

   --------------------
   -- Get_Field_Type --
   --------------------

   function Get_Field_Type (Item  : IDL_Type;
                            Index : Positive)
                            return IDL_Type
   is
   begin
      return Item.Fields.Element (Index).Field_Type;
   end Get_Field_Type;

   --------------
   -- Get_High --
   --------------

   function Get_High (Item : IDL_Type) return Natural is
   begin
      return Item.High;
   end Get_High;

   -----------------------
   -- Get_Literal_Count --
   -----------------------

   function Get_Literal_Count (Item : IDL_Type) return Natural
   is (Item.Fields.Last_Index);

   ----------------------
   -- Get_Literal_Name --
   ----------------------

   function Get_Literal_Name
     (Item  : IDL_Type;
      Index : Positive)
      return String
   is (Ada.Strings.Unbounded.To_String
         (Item.Fields.Element (Index).Field_Name));

   -------------
   -- Get_Low --
   -------------

   function Get_Low (Item : IDL_Type) return Natural is
   begin
      return Item.Low;
   end Get_Low;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : IDL_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Get_Name;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Item : IDL_Type) return Natural is
   begin
      return Item.Size;
   end Get_Size;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Name : String) return IDL_Type is
      Key : constant Ada.Strings.Unbounded.Unbounded_String :=
              Ada.Strings.Unbounded.To_Unbounded_String (Name);
   begin
      if Type_Table.Contains (Key) then
         return Type_Table.Element (Key);
      else
         return No_Type;
      end if;
   end Get_Type;

   --------------------
   -- Has_Count_Type --
   --------------------

   function Has_Count_Type (Item : IDL_Type) return Boolean
   is (Item.Count_Type /= null);

   -----------------------
   -- Internal_Add_Type --
   -----------------------

   procedure Internal_Add_Type
     (Name          : String;
      Ada_Package   : String;
      Ada_Name      : String;
      Size          : Natural  := 0;
      Derived_From  : IDL_Type := null;
      Count_Type    : IDL_Type := null;
      Low           : Integer := 0;
      High          : Integer := 0;
      Is_Record     : Boolean := False;
      Is_Interface  : Boolean := False;
      Is_Capability : Boolean := False;
      Is_Address    : Boolean := False;
      Is_Internal   : Boolean := False;
      Is_Range      : Boolean := False;
      Is_Scalar     : Boolean := True)
   is
      use Ada.Strings.Unbounded;
   begin
      if Get_Type (Name) = No_Type then
         Type_Table.Insert (Key      => To_Unbounded_String (Name),
                            New_Item => new IDL_Type_Record'
                              (To_Unbounded_String (Name),
                               To_Unbounded_String (Ada_Package),
                               To_Unbounded_String (Ada_Name),
                               Record_Field_Vectors.Empty_Vector,
                               Derived_From,
                               Size           =>
                                 (if Size = 0 then Default_Size else Size),
                               Low            => Low,
                               High           => High,
                               Count_Type     => Count_Type,
                               Is_Range       => Is_Range,
                               Is_Enumeration => False,
                               Is_Record      => Is_Record,
                               Is_Interface   => Is_Interface,
                               Is_Capability  => Is_Capability,
                               Is_Address     => Is_Address,
                               Is_Internal    => Is_Internal,
                               Is_Scalar      => Is_Scalar));
      end if;
   end Internal_Add_Type;

   ----------------
   -- Is_Address --
   ----------------

   function Is_Address (Item : IDL_Type) return Boolean is
   begin
      return Item.Is_Address;
   end Is_Address;

   -------------------
   -- Is_Capability --
   -------------------

   function Is_Capability (Item : IDL_Type) return Boolean is
   begin
      return Item.Is_Capability;
   end Is_Capability;

   function Is_Enumerated_Type (Item : IDL_Type) return Boolean
   is (Item.Is_Enumeration);

   ------------------
   -- Is_Interface --
   ------------------

   function Is_Interface (Item : IDL_Type) return Boolean is
   begin
      return Item.Is_Interface;
   end Is_Interface;

   -------------------
   -- Is_Range_Type --
   -------------------

   function Is_Range_Type (Item : IDL_Type) return Boolean is
   begin
      return Item.Is_Range;
   end Is_Range_Type;

   --------------------
   -- Is_Record_Type --
   --------------------

   function Is_Record_Type (Item : IDL_Type) return Boolean is
   begin
      return Item.Is_Record;
   end Is_Record_Type;

   ---------------
   -- Is_Scalar --
   ---------------

   function Is_Scalar (Item : IDL_Type) return Boolean is
   begin
      return Item.Is_Scalar;
   end Is_Scalar;

   ---------------
   -- Is_Stream --
   ---------------

   function Is_Stream (Item : IDL_Type) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      return Item.Name = "stream_element_array";
   end Is_Stream;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (Item : IDL_Type) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      return Item.Name = "string";
   end Is_String;

   -------------------------
   -- New_Enumerated_Type --
   -------------------------

   function New_Enumerated_Type
     (Interface_Name : String)
      return IDL_Type
   is
      use Ada.Strings.Unbounded;
   begin
      return new IDL_Type_Record'
        (Ada_Package    => +("Rose.Interfaces." & Interface_Name),
         Is_Enumeration => True,
         Is_Record      => False,
         Is_Interface   => False,
         Is_Capability  => False,
         Is_Address     => False,
         Is_Scalar      => True,
         others => <>);
   end New_Enumerated_Type;

   --------------------
   -- New_Range_Type --
   --------------------

   function New_Range_Type
     (Interface_Name : String;
      Low, High      : Integer)
      return IDL_Type
   is
      use Ada.Strings.Unbounded;
   begin
      return new IDL_Type_Record'
        (Ada_Package    => +("Rose.Interfaces." & Interface_Name),
         Is_Range       => True,
         Is_Scalar      => True,
         Low            => Low,
         High           => High,
         others         => <>);
   end New_Range_Type;

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type return IDL_Type is
   begin
      return new IDL_Type_Record'(Is_Record => True, others => <>);
   end New_Record_Type;

end IDL.Types;
