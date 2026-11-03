with Rose.Command_Arguments;

package body Ada.Command_Line is

   --------------
   -- Argument --
   --------------

   function Argument (Number : Positive) return String is
   begin
      if Number > Rose.Command_Arguments.Argument_Count then
         raise Constraint_Error;
      end if;

      return Rose.Command_Arguments.Argument (Number);
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural
   is (Rose.Command_Arguments.Argument_Count);

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name return String
   is (Rose.Command_Arguments.Command_Name);

   ---------------------
   -- Set_Exit_Status --
   ---------------------

   procedure Set_Exit_Status (Code : Exit_Status) is
   begin
      Rose.Command_Arguments.Set_Exit_Status (Integer (Code));
   end Set_Exit_Status;

end Ada.Command_Line;
