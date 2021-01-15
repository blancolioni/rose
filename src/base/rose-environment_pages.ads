private with Rose.Words;

package Rose.Environment_Pages is

   Environment_Base              : constant := 16#E000_0000#;
   Environment_Bound             : constant := 16#E100_0000#;

   type Environment_Page is private;

   procedure Clear (Page : in out Environment_Page);

   procedure Get_Environment_Value
     (Page  : Environment_Page;
      Name  : String;
      Value : out String;
      Last  : out Natural);

   procedure Insert_Environment_Value
     (Page     : in out Environment_Page;
      Name     : String;
      Value    : String;
      Inserted : out Boolean);

   procedure Set_Environment_Value
     (Page     : in out Environment_Page;
      Name     : String;
      Value    : String);

   procedure Delete_Environment_Value
     (Page  : in out Environment_Page;
      Name  : String);

private

   Max_Values_Per_Page : constant := 256;

   type Values_Per_Page_Range is range 1 .. Max_Values_Per_Page;

   type Value_Index_Record is
      record
         Start  : Rose.Words.Word_16;
         Length : Rose.Words.Word_16;
      end record;

   type Value_Indices is array (Values_Per_Page_Range) of Value_Index_Record;

   type Environment_Page is
      record
         Value_Count : Rose.Words.Word_32 := 0;
         Text_Length : Rose.Words.Word_32 := 0;
         Indices     : Value_Indices      := (others => (0, 0));
         Text        : String (1 .. 3064) := (others => Character'Val (0));
      end record;

   for Environment_Page'Size use 8 * 4096;

end Rose.Environment_Pages;
