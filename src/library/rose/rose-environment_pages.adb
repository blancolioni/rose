package body Rose.Environment_Pages is

   ---------------------------
   -- Get_Environment_Value --
   ---------------------------

   procedure Get_Environment_Value
     (Page  : Environment_Page;
      Name  : String;
      Value : out String;
      Last  : out Natural)
   is
      pragma Unreferenced (Page, Name);
   begin
      Value := (others => ' ');
      Last := 0;
   end Get_Environment_Value;

   ---------------------------
   -- Set_Environment_Value --
   ---------------------------

   procedure Set_Environment_Value
     (Page  : in out Environment_Page;
      Name  : String;
      Value : String)
   is
   begin
      null;
   end Set_Environment_Value;

end Rose.Environment_Pages;
