with Rose.Boot.Console;

with Rose.Words;

with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Capabilities.Meta is

   procedure Create_Endpoint
     (Params : Rose.Invocation.Invocation_Access);

   ---------------------
   -- Create_Endpoint --
   ---------------------

   procedure Create_Endpoint
     (Params : Rose.Invocation.Invocation_Access)
   is
      use Rose.Capabilities;
      use Rose.Invocation;
      Process_Id     : constant Rose.Kernel.Processes.Process_Id :=
                         Rose.Kernel.Processes.Current_Process_Id;
      Endpoint_Id    : Rose.Objects.Endpoint_Id;
      Local_Endpoint : Rose.Objects.Endpoint_Index  := 0;
      Identifier     : Rose.Objects.Capability_Identifier := 0;
      Receive_Cap    : Rose.Capabilities.Capability := Null_Capability;
      Endpoint_Cap   : Rose.Capabilities.Capability := Null_Capability;
   begin
      if not Params.Control.Flags (Recv_Caps)
        or else not Params.Control.Flags (Send_Words)
      then
         Rose.Kernel.Processes.Return_Error
           (Params, Request_Error);
         return;
      end if;

      Endpoint_Id := Rose.Objects.Endpoint_Id (Params.Data (0));

      if Params.Control.Last_Sent_Word >= 1 then
         Endpoint_Id := Endpoint_Id
           + 2 ** 32 * Rose.Objects.Endpoint_Id (Params.Data (1));
      end if;

      if Params.Control.Last_Sent_Word >= 2 then
         Identifier := Rose.Objects.Capability_Identifier (Params.Data (2));
      end if;

      if Endpoint_Id /= 0 then
         Local_Endpoint :=
           Rose.Kernel.Processes.Require_Endpoint
             (Process_Id, Endpoint_Id);
         if Local_Endpoint = 0 then
            Rose.Kernel.Processes.Debug.Put (Process_Id);
            Rose.Boot.Console.Put_Line (": out of endpoints");
            Rose.Kernel.Processes.Return_Error
              (Params, Rose.Invocation.Out_Of_Endpoints);
            return;
         end if;
      end if;

      if Endpoint_Id = 0 or else Params.Control.Last_Recv_Cap > 0 then
         Receive_Cap := Rose.Kernel.Processes.Create_Cap (Process_Id);

         if Receive_Cap = Null_Capability then
            Rose.Kernel.Processes.Return_Error
              (Params, Rose.Invocation.Out_Of_Capabilities);
            return;
         end if;

         if Log_Invocation then
            Rose.Kernel.Processes.Debug.Put (Process_Id);
            Rose.Boot.Console.Put
              (": new receive cap: ");
            Rose.Boot.Console.Put
              (Rose.Words.Word_8 (Receive_Cap));
            Rose.Boot.Console.New_Line;
         end if;

         Rose.Kernel.Processes.Set_Cap
           (Process_Id, Receive_Cap,
            Rose.Capabilities.Layout.Receive_Capability
              (Rose.Kernel.Processes.To_Object_Id (Process_Id),
               Local_Endpoint));
      end if;

      if Endpoint_Id /= 0 then
         Endpoint_Cap :=
           Rose.Kernel.Processes.Create_Endpoint_Cap
             (Process_Id, Endpoint_Id);

         if Endpoint_Cap = Null_Capability then
            if Receive_Cap /= Null_Capability then
               Rose.Kernel.Processes.Delete_Cap (Process_Id, Receive_Cap);
            end if;
            Rose.Kernel.Processes.Return_Error
              (Params, Rose.Invocation.Out_Of_Capabilities);
            return;
         end if;

         if Log_Endpoint_Cap_Create
           or else Log_Invocation
         then
            Rose.Kernel.Processes.Debug.Put (Process_Id);
            Rose.Boot.Console.Put
              (": new endpoint cap: ");
            Rose.Boot.Console.Put
              (Rose.Words.Word_8 (Endpoint_Cap));
            Rose.Boot.Console.Put (" for endpoint ");
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Local_Endpoint));
            Rose.Boot.Console.Put (" ");
            Rose.Boot.Console.Put (Endpoint_Id);
            Rose.Boot.Console.New_Line;
         end if;

         Rose.Kernel.Processes.Set_Cap
           (Process_Id, Endpoint_Cap,
            Rose.Capabilities.Layout.Endpoint_Capability
              (Rose.Kernel.Processes.To_Object_Id (Process_Id),
               Local_Endpoint, Identifier));
      end if;

      if Receive_Cap /= Null_Capability then
         Rose.Kernel.Processes.Set_Receive_Cap
           (Process_Id, Local_Endpoint, Receive_Cap);
      end if;

      Params.Control.Last_Sent_Cap := 0;

      if Receive_Cap /= Null_Capability then
         Params.Caps (0) := Receive_Cap;
         if Endpoint_Cap /= Null_Capability then
            Params.Caps (1) := Endpoint_Cap;
            Params.Control.Last_Sent_Cap := 1;
         end if;
      else
         Params.Caps (0) := Endpoint_Cap;
      end if;

      Params.Control.Flags :=
        (Rose.Invocation.Reply     => True,
         Rose.Invocation.Send_Caps => True,
         others                    => False);

      Rose.Kernel.Processes.Set_Current_State
        (Process_Id, Rose.Kernel.Processes.Ready);

   end Create_Endpoint;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      Process_Id : constant Rose.Kernel.Processes.Process_Id :=
                     Rose.Kernel.Processes.Current_Process_Id;
   begin
      case Cap.Header.Endpoint is
         when Create_Endpoint_Endpoint =>
            Create_Endpoint (Params);
--           when Create_Page_Object_Cap_Endpoint =>
--              declare
--                 use Rose.Capabilities;
--                 use Rose.Invocation;
--                 use Rose.Words;
--                 Flags   : constant Word := Params.Data (2);
--                 New_Cap : constant Capability :=
--                             Rose.Kernel.Processes.Create_Cap;
--                 Page_Object : constant Rose.Objects.Page_Object_Id :=
--                                 Rose.Kernel.Processes.Get_Page_Object_Id
--                                   (Rose.Kernel.Processes.Current_Process_Id,
--                                    Params.Data (1));
--              begin
--                 if New_Cap = Null_Capability then
--                    Rose.Kernel.Processes.Return_Error
--                      (Rose.Invocation.Out_Of_Capabilities);
--                 else
--                    Rose.Kernel.Processes.Set_Cap
--                      (New_Cap,
--                       Rose.Capabilities.Layout.Make_Page_Object_Cap
--                         (Readable         => (Flags and 1) = 1,
--                          Writable         => (Flags and 2) = 2,
--                          To_Shared_Buffer => (Flags and 2) = 2,
--                          Page_Object      => Page_Object),
--                       Default => False);
--
--                    Params.Control.Flags :=
--                      (Reply => True, others => False);
--                    if Params.Control.Recv_Word_Count > 0 then
--                       Params.Control.Sent_Word_Count := 1;
--                       Params.Control.Recv_Word_Count := 0;
--                       Params.Control.Cap_Word_Count  := 1;
--                       Params.Data (1) := Word_32 (New_Cap);
--                    else
--                       Params.Control.Sent_Word_Count := 0;
--                    end if;
--                    Rose.Kernel.Processes.Set_Current_State
--                      (Rose.Kernel.Processes.Ready);
--                 end if;
--              end;

         when Receive_On_Any_Cap =>
            Rose.Kernel.Processes.Receive_Any
              (Rose.Kernel.Processes.Current_Process_Id,
               Params.all);

         when Rescind_Cap =>
            if Params.Control.Flags (Rose.Invocation.Send_Caps) then
               for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
                  Rose.Kernel.Processes.Rescind_Cap
                    (Rose.Kernel.Processes.Current_Process_Id,
                     Params.Caps (Cap_Index));
               end loop;
               Params.Control.Flags :=
                 (Rose.Invocation.Reply     => True,
                  others                    => False);
            else
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Invalid_Operation);
            end if;

            Rose.Kernel.Processes.Set_Current_State
              (Process_Id, Rose.Kernel.Processes.Ready);

         when Delete_Cap =>
            if Params.Control.Flags (Rose.Invocation.Send_Caps) then
               for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
                  Rose.Kernel.Processes.Delete_Cap
                    (Rose.Kernel.Processes.Current_Process_Id,
                     Params.Caps (Cap_Index));
               end loop;
               Params.Control.Flags :=
                 (Rose.Invocation.Reply     => True,
                  others                    => False);
            else
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Invalid_Operation);
            end if;

            Rose.Kernel.Processes.Set_Current_State
              (Process_Id, Rose.Kernel.Processes.Ready);

         when Enable_Kernel_Debug =>
            declare
               use type Rose.Words.Word;
               Enabled : constant Boolean :=
                           Params.Data (1) /= 0;
            begin
               Log_Invocation := Enabled;
               Log_Reply := Enabled;
            end;

         when Exit_Process =>
            Rose.Kernel.Processes.Set_Current_State
              (Process_Id, Rose.Kernel.Processes.Killed);

         when others =>
            Rose.Kernel.Processes.Return_Error
              (Params, Rose.Invocation.Invalid_Endpoint);
      end case;
   end Handle;

end Rose.Kernel.Capabilities.Meta;
