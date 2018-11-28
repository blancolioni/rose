with Rose.Words;

with Rose.Kernel.Processes;

with Rose.Kernel.Debug;
with Rose.Boot.Console;

package body Rose.Kernel.Capabilities.Create is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      pragma Unreferenced (Cap);
      use Rose.Invocation, Rose.Words;
      use type Rose.Capabilities.Capability;
      Process_Id   : constant Rose.Kernel.Processes.Process_Id :=
                       Rose.Kernel.Processes.Current_Process_Id;
      New_Cap      : Rose.Capabilities.Capability;
      Cap_Layout   : Rose.Capabilities.Layout.Capability_Layout;
      Cap_Array    : array (Parameter_Word_Index range 0 .. 3) of Word_32;
      for Cap_Array'Address use Cap_Layout'Address;
      pragma Import (Ada, Cap_Array);
   begin

      if not Params.Control.Flags (Send_Words)
        or else Params.Control.Last_Sent_Word /= 3
      then
         Rose.Kernel.Processes.Return_Error
           (Params, Rose.Invocation.Invalid_Operation);
         return;
      end if;

      for I in Cap_Array'Range loop
         Cap_Array (I) := Params.Data (I);
      end loop;

      New_Cap := Rose.Kernel.Processes.Create_Cap (Process_Id);

      if New_Cap = Rose.Capabilities.Null_Capability then
         Rose.Kernel.Processes.Return_Error
           (Params, Rose.Invocation.Out_Of_Capabilities);
         return;
      end if;

      Rose.Kernel.Processes.Set_Cap
        (Pid    => Process_Id,
         Cap    => New_Cap,
         Layout => Cap_Layout);

      Params.Control.Flags :=
        (Reply => True, Send_Caps => True, others => False);
      Params.Control.Last_Sent_Cap := 0;
      Params.Caps (0) := New_Cap;

      Rose.Boot.Console.Put ("create-cap: ");
      Rose.Boot.Console.Put (Rose.Words.Word_8 (New_Cap));
      Rose.Boot.Console.Put (" ");
      Rose.Kernel.Debug.Put_Cap_Type (Cap_Layout.Header.Cap_Type);
      Rose.Boot.Console.Put (" ");
      Rose.Boot.Console.Put (Rose.Words.Word_16 (Cap_Layout.Header.Endpoint));
      Rose.Boot.Console.Put (" ");
      Rose.Boot.Console.Put (Rose.Words.Word (Cap_Layout.Payload));
      Rose.Boot.Console.New_Line;

      Rose.Kernel.Processes.Set_Current_State
        (Process_Id, Rose.Kernel.Processes.Ready);

   end Handle;

end Rose.Kernel.Capabilities.Create;
