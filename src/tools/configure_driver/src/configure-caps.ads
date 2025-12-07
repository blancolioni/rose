private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Vectors;

package Configure.Caps is

   type Cap_Config (<>) is tagged private;

   function Name (This : Cap_Config) return String;
   function Field_Count (This : Cap_Config) return Natural;
   function Field
     (This  : Cap_Config;
      Index : Positive)
      return String;
   function Field
     (This  : Cap_Config;
      Index : Positive)
      return Integer;

   function Create
     (Name : String;
      Field_1 : String := "";
      Field_2 : String := "";
      Field_3 : String := "";
      Field_4 : String := "")
      return Cap_Config;

   type Cap_Config_List is tagged private;

   procedure Iterate
     (This    : Cap_Config_List;
      Process : not null access
        procedure (Cap : Cap_Config));

   procedure Append
     (This : in out Cap_Config_List;
      Cap  : Cap_Config'Class);

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Cap_Config (Key_Length : Natural) is tagged
      record
         Key    : String (1 .. Key_Length);
         Fields : String_Vectors.Vector;
      end record;

   function Name (This : Cap_Config) return String
   is (This.Key);

   function Field_Count (This : Cap_Config) return Natural
   is (This.Fields.Last_Index);

   function Field
     (This  : Cap_Config;
      Index : Positive)
      return String
   is (This.Fields.Element (Index));

   function Field
     (This  : Cap_Config;
      Index : Positive)
      return Integer
   is (Integer'Value (This.Fields.Element (Index)));

   package Cap_Config_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Cap_Config);

   type Cap_Config_List is tagged
      record
         List : Cap_Config_Lists.List;
      end record;

end Configure.Caps;
