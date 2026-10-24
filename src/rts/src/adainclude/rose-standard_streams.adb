with System.Storage_Elements;

with Rose.Capabilities.Standard;
with Rose.Interfaces.Cap_Set;
with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.Words;

package body Rose.Standard_Streams is

   Input_Cap  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Output_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Error_Cap  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   Known : Boolean := False;
   --  True once the streams have been found or set.

   Invoke_Buffer_Length : constant := 4096;
   Invoke_Buffer        : System.Storage_Elements.Storage_Array
     (1 .. Invoke_Buffer_Length)
     with Alignment => 4096;
   --  The buffer through which Ada.Text_IO sends and receives text.

   procedure Load;
   --  Find the streams in the argument cap set, unless they are known.

   procedure Set_Known;
   --  Note that the streams are known, and give the system calls a buffer
   --  for the text sent through them.

   ----------
   -- Load --
   ----------

   procedure Load is

      Get_Cap : Rose.Capabilities.Capability;

      function Get_Argument_Cap
        (Index : Rose.Words.Word)
         return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
            (Cap  => Get_Cap,
             Data => (1 => Index)));

   begin
      if Known then
         return;
      end if;

      Set_Known;

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send
           (Params, Rose.Capabilities.Standard.Argument_Cap_Set);
         Rose.System_Calls.Receive_Caps
           (Params, Rose.Interfaces.Cap_Set.Subprogram_Count);
         Rose.System_Calls.Invoke_Capability (Params);
         Get_Cap := Params.Caps (Rose.Interfaces.Cap_Set.Get_Cap_Index);
      end;

      --  The cap set holds the input stream's interface, from which the
      --  Read cap comes, and the output streams' Write caps.

      Input_Cap :=
        Rose.System_Calls.Client.Get_Capability
          (Get_Argument_Cap (1), (1 => 0));
      Output_Cap := Get_Argument_Cap (2);
      Error_Cap := Get_Argument_Cap (3);
   end Load;

   ---------
   -- Set --
   ---------

   procedure Set
     (Standard_Input  : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability;
      Standard_Output : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability;
      Standard_Error  : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
   is
   begin
      Set_Known;
      Input_Cap := Standard_Input;
      Output_Cap := Standard_Output;
      Error_Cap := Standard_Error;
   end Set;

   ---------------
   -- Set_Known --
   ---------------

   procedure Set_Known is
   begin
      if not Known then
         Known := True;
         Rose.System_Calls.Use_Buffer
           (Invoke_Buffer'Address, Invoke_Buffer_Length);
      end if;
   end Set_Known;

   ------------------------
   -- Set_Standard_Error --
   ------------------------

   procedure Set_Standard_Error (Cap : Rose.Capabilities.Capability) is
   begin
      Set_Known;
      Error_Cap := Cap;
   end Set_Standard_Error;

   ------------------------
   -- Set_Standard_Input --
   ------------------------

   procedure Set_Standard_Input (Cap : Rose.Capabilities.Capability) is
   begin
      Set_Known;
      Input_Cap := Cap;
   end Set_Standard_Input;

   -------------------------
   -- Set_Standard_Output --
   -------------------------

   procedure Set_Standard_Output (Cap : Rose.Capabilities.Capability) is
   begin
      Set_Known;
      Output_Cap := Cap;
   end Set_Standard_Output;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return Rose.Capabilities.Capability is
   begin
      Load;
      return Error_Cap;
   end Standard_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return Rose.Capabilities.Capability is
   begin
      Load;
      return Input_Cap;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return Rose.Capabilities.Capability is
   begin
      Load;
      return Output_Cap;
   end Standard_Output;

end Rose.Standard_Streams;
