with System;

with Rose.Environment_Pages;

package body Rose.Environment is

   Start_Environment_Page : Rose.Environment_Pages.Environment_Page;
   pragma Import (Ada, Start_Environment_Page);
   for Start_Environment_Page'Address use
     System'To_Address (Rose.Environment_Pages.Environment_Base);

   ---------------------------
   -- Get_Environment_Value --
   ---------------------------

   procedure Get_Environment_Value
     (Name  : String;
      Value : out String;
      Last  : out Natural)
   is
   begin
      Rose.Environment_Pages.Get_Environment_Value
        (Start_Environment_Page, Name, Value, Last);
   end Get_Environment_Value;

   ---------------------------
   -- Set_Environment_Value --
   ---------------------------

   procedure Set_Environment_Value
     (Name  : String;
      Value : String)
   is
   begin
      Rose.Environment_Pages.Set_Environment_Value
        (Start_Environment_Page, Name, Value);
   end Set_Environment_Value;

end Rose.Environment;
