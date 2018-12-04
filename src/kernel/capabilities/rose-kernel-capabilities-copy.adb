with Rose.Kernel.Processes;
with Rose.Words;
with Rose.Boot.Console;

with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Capabilities.Copy is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      use Rose.Capabilities;
      use type Rose.Objects.Endpoint_Id;
      use type Rose.Invocation.Parameter_Word_Index;
      To_Process_Id   : constant Rose.Kernel.Processes.Process_Id :=
                            Rose.Kernel.Processes.Current_Process_Id;
      From_Process_Id : constant Rose.Kernel.Processes.Process_Id :=
                          Rose.Kernel.Processes.To_Process_Id
                            (Cap.Payload);
      Endpoint_Id     : constant Rose.Objects.Endpoint_Id :=
                          Rose.Objects.Endpoint_Id (Params.Data (0))
                          + 2 ** 32
                            * Rose.Objects.Endpoint_Id (Params.Data (1));
      Update_Id : constant Boolean := Params.Control.Last_Sent_Word >= 2;
      Cap_Id : constant Rose.Objects.Capability_Identifier :=
                 (if Update_Id
                  then Rose.Objects.Capability_Identifier (Params.Data (2))
                  else 0);

      Endpoint_Cap : constant Rose.Capabilities.Capability :=
                       Rose.Kernel.Processes.Find_Endpoint_Cap
                         (From_Process_Id, Endpoint_Id);
      Local_Cap       : Rose.Capabilities.Capability :=
                          Rose.Capabilities.Null_Capability;
   begin

      if Log_Cap_Copy then
         Rose.Boot.Console.Put ("copy-cap: endpoint = ");
         Rose.Boot.Console.Put (Params.Data (1));
         Rose.Boot.Console.Put (" ");
         Rose.Boot.Console.Put (Params.Data (0));
         Rose.Boot.Console.Put ("; pid = ");
         Rose.Kernel.Processes.Debug.Put (From_Process_Id);
         Rose.Boot.Console.Put ("; cap = ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Endpoint_Cap));
         Rose.Boot.Console.New_Line;
      end if;

      if Endpoint_Cap = Null_Capability then
         Rose.Boot.Console.Put ("pid ");
         Rose.Kernel.Processes.Debug.Put (From_Process_Id);
         Rose.Boot.Console.Put (": no such endpoint: ");
         Rose.Boot.Console.Put (Params.Data (1));
         Rose.Boot.Console.Put (" ");
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
        (From_Process_Id => From_Process_Id,
         From_Cap        => Endpoint_Cap,
         To_Process_Id   => To_Process_Id,
         To_Cap          => Local_Cap);

      if Update_Id then
         Rose.Kernel.Processes.Set_Cap_Id
           (To_Process_Id, Local_Cap, Cap_Id);
      end if;

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
