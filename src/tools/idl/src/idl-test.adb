with IDL.Generate;
with IDL.Syntax;

package body IDL.Test is

   function Signal_Interface return IDL.Syntax.IDL_Interface;

   ----------------------
   -- Signal_Interface --
   ----------------------

   function Signal_Interface return IDL.Syntax.IDL_Interface is
      use IDL.Syntax;
      Result : IDL_Interface;
      Subpr  : IDL_Subprogram;
   begin
      Result := New_Interface ("signal");
      Add_Context (Result, "Rose.Words");
      Subpr := New_Subprogram (Result, "receive_signal");
      Add_Argument (Subpr, "Payload", Word_32_Type);
      return Result;
   end Signal_Interface;

   --------------
   -- Test_IDL --
   --------------

   procedure Test_IDL is
      Test_Interface : constant IDL.Syntax.IDL_Interface :=
                         Signal_Interface;
   begin
      IDL.Generate.Generate_Interface (Test_Interface);
   end Test_IDL;

end IDL.Test;
