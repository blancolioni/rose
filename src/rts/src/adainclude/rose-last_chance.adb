package body Rose.Last_Chance is

   Current_Handler : Failure_Handler := null;

   -------------
   -- Handler --
   -------------

   function Handler return Failure_Handler is
   begin
      return Current_Handler;
   end Handler;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler (Handler : Failure_Handler) is
   begin
      Current_Handler := Handler;
   end Set_Handler;

end Rose.Last_Chance;
