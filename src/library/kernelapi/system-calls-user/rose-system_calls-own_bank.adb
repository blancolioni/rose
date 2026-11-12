with Rose.Capabilities.Standard;
with Rose.Invocation;

with Rose.Interfaces.Process;

package body Rose.System_Calls.Own_Bank is

   use type Rose.Capabilities.Capability;

   Asked     : Boolean := False;
   Bank_Cap  : Rose.Capabilities.Capability :=
                 Rose.Capabilities.Null_Capability;

   ---------
   -- Cap --
   ---------

   function Cap return Rose.Capabilities.Capability is
      use Rose.Invocation;
      Params   : aliased Invocation_Record;
      Endpoint : Rose.Capabilities.Capability :=
                   Rose.Capabilities.Null_Capability;
   begin
      if Asked then
         return Bank_Cap;
      end if;
      Asked := True;

      --  The Process interface gives the Bank endpoint, and the Bank
      --  endpoint gives the bank.

      Endpoint :=
        Get_Interface_Cap
          (Rose.Capabilities.Standard.Process_Interface_Cap,
           Rose.Interfaces.Process.Bank_Index);

      if Endpoint /= Rose.Capabilities.Null_Capability then
         Initialize_Send (Params, Endpoint);
         Receive_Caps (Params, 1);
         Invoke_Capability (Params);
         if not Params.Control.Flags (Error) then
            Bank_Cap := Params.Caps (0);
         end if;
      end if;

      return Bank_Cap;
   end Cap;

end Rose.System_Calls.Own_Bank;
