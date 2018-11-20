with Rose.Kernel.Processes;
with Rose.Kernel.Validation;
with Rose.Words;
with Rose.Boot.Console;

package body Rose.Kernel.Capabilities.Copy is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      use Rose.Capabilities;
      To_Process_Id   : constant Rose.Objects.Process_Id :=
                            Rose.Kernel.Processes.Current_Process_Id;
      From_Process_Id : constant Rose.Objects.Process_Id :=
                          Rose.Objects.To_Process_Id (Cap.Payload);
      Endpoint_Cap : constant Rose.Capabilities.Capability :=
                       Rose.Kernel.Processes.Find_Endpoint_Cap
                         (From_Process_Id,
                          Rose.Objects.Endpoint_Id (Params.Data (0)));
      Local_Cap       : Rose.Capabilities.Capability :=
                          Rose.Capabilities.Null_Capability;
   begin

      Rose.Boot.Console.Put ("copy-cap: endpoint = ");
      Rose.Boot.Console.Put (Params.Data (0));
      Rose.Boot.Console.Put ("; pid = ");
      Rose.Boot.Console.Put (Rose.Words.Word_8 (From_Process_Id));
      Rose.Boot.Console.Put ("; cap = ");
      Rose.Boot.Console.Put (Rose.Words.Word_8 (Endpoint_Cap));
      Rose.Boot.Console.New_Line;

      if Endpoint_Cap = Null_Capability then
         Rose.Boot.Console.Put ("pid ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (From_Process_Id));
         Rose.Boot.Console.Put (": no such endpoint: ");
         Rose.Boot.Console.Put (Params.Data (0));
         Rose.Boot.Console.New_Line;

         Rose.Kernel.Processes.Return_Error
           (Params, Rose.Invocation.Invalid_Endpoint);
         return;
      end if;

      Local_Cap :=
        Rose.Kernel.Processes.Create_Cap (To_Process_Id);

      if Local_Cap = Null_Capability then
         Rose.Kernel.Processes.Return_Error
           (Params, Rose.Invocation.Out_Of_Capabilities);
         return;
      end if;

      Rose.Kernel.Processes.Copy_Cap_Layout
        (From_Process => From_Process_Id,
         From_Cap     => Endpoint_Cap,
         To_Process   => To_Process_Id,
         To_Cap       => Local_Cap);

      Rose.Kernel.Validation.Create_Cap
        (To_Process_Id, Local_Cap,
         Rose.Kernel.Processes.Cap_Type
           (To_Process_Id, Local_Cap));
      Params.Control.Flags :=
        (Rose.Invocation.Reply     => True,
         Rose.Invocation.Send_Caps => True,
         others                    => False);

      Params.Control.Last_Sent_Cap := 0;
      Params.Caps (0) := Local_Cap;

      Rose.Kernel.Processes.Set_Current_State
        (To_Process_Id, Rose.Kernel.Processes.Ready);

   end Handle;

end Rose.Kernel.Capabilities.Copy;
