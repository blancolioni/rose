with System.Storage_Elements;

with Rose.Words;

with Rose.Devices;

with Rose.Kernel.Processes;
with Rose.Kernel.Panic;

with Rose.Boot.Console;

with Rose.Kernel.Capabilities.Processes;

with Rose.Kernel.Clock;
with Rose.Kernel.Heap;

with Rose.Kernel.Checkpoint;

with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Capabilities.Kernel_Caps is

   function Process_Cap
     (Pid : Rose.Kernel.Processes.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Rose.Capabilities.Layout.Capability_Layout;

   function Process_Interface_Cap
     (Pid : Rose.Kernel.Processes.Process_Id)
      return Rose.Capabilities.Layout.Capability_Layout
   is (Process_Cap (Pid, Processes.Process_Interface_Endpoint));

   function Start_Process_Cap
     (Pid : Rose.Kernel.Processes.Process_Id)
      return Rose.Capabilities.Layout.Capability_Layout
   is (Process_Cap (Pid, Processes.Start_Process_Endpoint));

   function Initial_Cap
     (Pid : Rose.Kernel.Processes.Process_Id)
      return Rose.Capabilities.Layout.Capability_Layout
   is (Process_Cap (Pid, Processes.Initial_Cap_Endpoint));

   procedure Write_Image
     (Current  : in out Rose.Objects.Object_Id;
      Storage  : out System.Storage_Elements.Storage_Array;
      Last     : out System.Storage_Elements.Storage_Offset;
      Complete : out Boolean);

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
   begin
      case Cap.Header.Endpoint is
         when Enable_Paging_Endpoint =>
            Rose.Kernel.Processes.Set_Process_Handlers
              (On_Launch     => Params.Caps (0),
               On_Kill       => Params.Caps (1),
               On_Page_Fault => Params.Caps (2));
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);
         when Start_Image_Write_Endpoint =>
            if not Params.Control.Flags (Rose.Invocation.Send_Buffer) then
               Rose.Boot.Console.Put_Line
                 ("kernel: no image buffer");
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Request_Error);
               return;
            end if;

            if not Params.Control.Flags (Rose.Invocation.Writable_Buffer) then
               Rose.Boot.Console.Put_Line
                 ("kernel: image buffer is not writable");
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Request_Error);
               return;
            end if;

            if not Image_Write_Active then
               Checkpoint_Active := True;
               Image_Write_Active := True;
               Image_Writer := Rose.Kernel.Processes.Current_Object_Id;
               Current_Image_Object := Rose.Objects.Null_Object_Id;
            end if;

            declare
               Storage : System.Storage_Elements.Storage_Array
                 (1 .. Params.Buffer_Length);
               pragma Import (Ada, Storage);
               for Storage'Address use Params.Buffer_Address;
               Last    : System.Storage_Elements.Storage_Count;
               Complete : Boolean;
            begin
               Write_Image (Current_Image_Object, Storage, Last, Complete);

               Params.all :=
                 (Control => (Flags => (Rose.Invocation.Reply => True,
                                        Rose.Invocation.Send_Words => True,
                                        others                     => False),
                              Last_Sent_Word => 1,
                              others         => <>),
                  Data    => (Rose.Words.Word (Last), Boolean'Pos (Complete),
                              others => 0),
                  others  => <>);
               Rose.Kernel.Processes.Set_Current_State
                 (Rose.Kernel.Processes.Current_Process_Id,
                  Rose.Kernel.Processes.Ready);
            end;

         when Create_Process_Endpoint =>
            declare
               use Rose.Kernel.Processes;
               Pid : constant Process_Id := New_Process;
            begin
               if Pid = Null_Process_Id then
                  Rose.Boot.Console.Put_Line ("kernel: out of processes");
                  Return_Error (Params,
                                Rose.Invocation.Invalid_Operation,
                                0);
                  Rose.Kernel.Processes.Set_Current_State
                    (Rose.Kernel.Processes.Current_Process_Id,
                     Rose.Kernel.Processes.Ready);
                  return;
               end if;

               Rose.Boot.Console.Put ("kernel: create process ");
               Rose.Kernel.Processes.Debug.Put (Pid);
               Rose.Boot.Console.New_Line;

               if Params.Control.Flags (Rose.Invocation.Send_Caps) then
                  for Index in 0 .. Params.Control.Last_Sent_Cap loop
                     Copy_Cap (Current_Process_Id, Pid,
                               Params.Caps (Index));
                  end loop;
               end if;

               Params.all :=
                 (Control =>
                    (Flags =>
                         (Rose.Invocation.Reply => True,
                          others                => False),
                     others         => <>),
                  others  => <>);

               Rose.Invocation.Send_Cap
                 (Params => Params.all,
                  Cap    =>
                    New_Cap
                      (Current_Process_Id,
                       Process_Interface_Cap (Pid)));
               Rose.Invocation.Send_Cap
                 (Params => Params.all,
                  Cap    =>
                    New_Cap
                      (Current_Process_Id,
                       Start_Process_Cap (Pid)));
               Rose.Invocation.Send_Cap
                 (Params => Params.all,
                  Cap    =>
                    New_Cap
                      (Current_Process_Id,
                       Initial_Cap (Pid)));

            end;

            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Add_Heap_Memory =>
            Rose.Kernel.Heap.Increase_Heap_Bound
              (Start  => Physical_Address (Params.Data (0)),
               Amount => Physical_Bytes (Params.Data (1)));

         when Set_Timeout_Endpoint =>
            if not Params.Control.Flags (Rose.Invocation.Send_Words)
              or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
            then
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Invalid_Operation);
               return;
            end if;

            declare
               Cap : constant Rose.Capabilities.Capability :=
                       Params.Caps (0);
               Timeout : constant Rose.Words.Word :=
                           Params.Data (0);
            begin
               Rose.Kernel.Clock.Set_Timeout
                 (Timeout, Rose.Kernel.Processes.Current_Object_Id, Cap);
            end;

            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Get_Current_Ticks_Endpoint =>
            if not Params.Control.Flags (Rose.Invocation.Recv_Words) then
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Invalid_Operation);
               return;
            end if;

            Params.all :=
              (Control =>
                 (Flags          =>
                      (Rose.Invocation.Reply => True,
                       others                     => False),
                  others         => <>),
               others  => <>);

            Rose.Invocation.Send_Word
              (Params.all,
               Rose.Kernel.Clock.Current_Ticks);

            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Enter_Checkpoint_Endpoint =>
            Rose.Kernel.Checkpoint.Enter_Checkpoint;
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Leave_Checkpoint_Endpoint =>
            Rose.Kernel.Checkpoint.Leave_Checkpoint;
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when others =>
            Rose.Kernel.Panic.Panic
              ("bad kernel cap endpoint");

      end case;
   end Handle;

   -----------------
   -- Process_Cap --
   -----------------

   function Process_Cap
     (Pid      : Rose.Kernel.Processes.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Rose.Capabilities.Layout.Capability_Layout
   is
      use Rose.Capabilities.Layout;
   begin
      return Capability_Layout'
        (Header  =>
           Capability_Header'
             (Cap_Type         => Process_Cap,
              Endpoint         => Endpoint,
              Identifier       => 0,
              others           => <>),
         Payload =>
           Rose.Kernel.Processes.To_Object_Id (Pid));
   end Process_Cap;

   -----------------
   -- Write_Image --
   -----------------

   procedure Write_Image
     (Current  : in out Rose.Objects.Object_Id;
      Storage  : out System.Storage_Elements.Storage_Array;
      Last     : out System.Storage_Elements.Storage_Offset;
      Complete : out Boolean)
   is
      pragma Unreferenced (Current);
      use System.Storage_Elements;
   begin
      Last := Storage'First - 1;
      Rose.Devices.To_Storage
        (Storage, Last, Rose.Words.Word_64'(System_Image_Start_Magic));
      Rose.Devices.To_Storage
        (Storage, Last, Rose.Words.Word_64'(System_Image_End_Magic));
      Complete := True;
   end Write_Image;

end Rose.Kernel.Capabilities.Kernel_Caps;
