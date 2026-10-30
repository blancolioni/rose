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
with Rose.Kernel.Processes.Snapshot;

with Rose.Arch.Interrupts;

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

   function Command_Endpoint_Cap
     (Pid : Rose.Kernel.Processes.Process_Id)
      return Rose.Capabilities.Layout.Capability_Layout
   is (Process_Cap (Pid, Processes.Command_Endpoint_Endpoint));

   procedure Write_Image
     (Current  : in out Rose.Objects.Object_Id;
      Storage  : out System.Storage_Elements.Storage_Array;
      Last     : out System.Storage_Elements.Storage_Offset;
      Complete : out Boolean);

   procedure Decode_Name
     (From : Rose.Invocation.Invocation_Access;
      Name : out String;
      Last : out Natural);

   -----------------
   -- Decode_Name --
   -----------------

   procedure Decode_Name
     (From : Rose.Invocation.Invocation_Access;
      Name : out String;
      Last : out Natural)
   is
   begin
      Last := 0;
      if not From.Control.Flags (Rose.Invocation.Send_Words) then
         return;
      end if;
      for Sent_Word_Index in 0 .. From.Control.Last_Sent_Word loop
         declare
            use type Rose.Words.Word;
            Word : Rose.Words.Word :=
                     From.Data (Sent_Word_Index);
         begin
            for I in 1 .. 4 loop
               declare
                  Ascii : constant Rose.Words.Word := Word mod 256;
               begin
                  if Ascii /= 0 then
                     Last := Last + 1;
                     Name (Last) := Character'Val (Ascii);
                  end if;
               end;
               Word := Word / 256;
               exit when Word = 0;
            end loop;
         end;
      end loop;
   end Decode_Name;

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
         when Start_Log_Endpoint =>
            Rose.Kernel.Checkpoint.Set_Log_Handlers
              (Create => Params.Caps (0),
               Append => Params.Caps (1),
               Commit => Params.Caps (2));
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
               use type Rose.Invocation.Parameter_Word_Index;
               Max_Name_Length : constant Natural :=
                                   (if Params.Control.Flags
                                      (Rose.Invocation.Send_Words)
                                    then Natural
                                      (Params.Control.Last_Sent_Word + 1) * 4
                                    else 0);
               Name : String (1 .. Max_Name_Length);
               Name_Last : Natural := 0;
            begin
               if Max_Name_Length > 0 then
                  Decode_Name (Params, Name, Name_Last);
               end if;

               declare
                  use Rose.Kernel.Processes;
                  Pid : constant Process_Id :=
                            New_Process (Name (1 .. Name_Last));
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

                  if Params.Control.Flags (Rose.Invocation.Send_Caps) then
                     for Index in 0 .. Params.Control.Last_Sent_Cap loop
                        Copy_Cap (Current_Process_Id, Pid,
                                  Params.Caps (Index));
                     end loop;
                  end if;

                  Params.all :=
                    (Control =>
                       (Flags          =>
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
                  Rose.Invocation.Send_Cap
                    (Params => Params.all,
                     Cap    =>
                       New_Cap
                         (Current_Process_Id,
                          Command_Endpoint_Cap (Pid)));

               end;

               Rose.Kernel.Processes.Set_Current_State
                 (Rose.Kernel.Processes.Current_Process_Id,
                  Rose.Kernel.Processes.Ready);
            end;

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

         when Get_Current_Ticks_64_Endpoint =>
            declare
               use type Rose.Invocation.Parameter_Word_Index;
               Low, High : Rose.Words.Word;
            begin
               if not Params.Control.Flags (Rose.Invocation.Recv_Words)
                 or else Params.Control.Last_Recv_Word < 1
               then
                  Rose.Kernel.Processes.Return_Error
                    (Params, Rose.Invocation.Invalid_Operation);
                  return;
               end if;

               Rose.Kernel.Clock.Current_Ticks_64 (Low, High);

               Params.all :=
                 (Control =>
                    (Flags          =>
                         (Rose.Invocation.Reply => True,
                          others                     => False),
                     others         => <>),
                  others  => <>);

               Rose.Invocation.Send_Word (Params.all, Low);
               Rose.Invocation.Send_Word (Params.all, High);
            end;

            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Trace_Endpoint =>
            if Trace_Launch
              and then Params.Control.Flags (Rose.Invocation.Send_Words)
            then
               declare
                  Text : String (1 .. 4 * Rose.Invocation.Max_Parameter_Words);
                  Last : Natural;
               begin
                  Decode_Name (Params, Text, Last);
                  Rose.Kernel.Processes.Debug.Put
                    (Rose.Kernel.Processes.Current_Process_Id);
                  Rose.Boot.Console.Put (": trace: ");
                  Rose.Boot.Console.Put (Text (1 .. Last));
                  Rose.Boot.Console.New_Line;
               end;
            end if;

            Params.all :=
              (Control =>
                 (Flags  =>
                      (Rose.Invocation.Reply => True,
                       others                => False),
                  others => <>),
               others  => <>);
            Rose.Invocation.Send_Word
              (Params.all, Rose.Kernel.Clock.Current_Ticks);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Enter_Checkpoint_Endpoint =>
            if not Params.Control.Flags (Rose.Invocation.Send_Buffer)
              or else not Params.Control.Flags
                (Rose.Invocation.Writable_Buffer)
            then
               Rose.Boot.Console.Put_Line
                 ("kernel: enter checkpoint needs a writable buffer");
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Request_Error);
               return;
            end if;

            declare
               Tables     : System.Storage_Elements.Storage_Array
                 (1 .. Params.Buffer_Length);
               pragma Import (Ada, Tables);
               for Tables'Address use Params.Buffer_Address;
               Page_Count : Natural;
               Success    : Boolean;
            begin
               Rose.Kernel.Checkpoint.Enter_Checkpoint
                 (Tables, Page_Count, Success);

               if not Success then
                  Rose.Kernel.Processes.Return_Error
                    (Params, Rose.Invocation.Request_Error);
                  return;
               end if;

               Params.all :=
                 (Control => (Flags => (Rose.Invocation.Reply => True,
                                        Rose.Invocation.Send_Words => True,
                                        others                     => False),
                              Last_Sent_Word => 0,
                              others         => <>),
                  Data    => (Rose.Words.Word (Page_Count), others => 0),
                  others  => <>);
               Rose.Kernel.Processes.Set_Current_State
                 (Rose.Kernel.Processes.Current_Process_Id,
                  Rose.Kernel.Processes.Ready);
            end;

         when Leave_Checkpoint_Endpoint =>
            Rose.Kernel.Checkpoint.Leave_Checkpoint;
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Restore_Process_Endpoint =>
            if not Params.Control.Flags (Rose.Invocation.Send_Buffer)
              or else not Params.Control.Flags (Rose.Invocation.Send_Words)
            then
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Request_Error);
               return;
            end if;

            declare
               use Rose.Kernel.Processes;
               Pages   : System.Storage_Elements.Storage_Array
                 (1 .. Params.Buffer_Length);
               pragma Import (Ada, Pages);
               for Pages'Address use Params.Buffer_Address;
               Pid     : Process_Id;
               Success : Boolean;
            begin
               Rose.Kernel.Processes.Snapshot.Restore_Process
                 (Pages          => Pages,
                  Next_Object_Id =>
                    Rose.Invocation.Get_Object_Id (Params.all, 0),
                  Pid            => Pid,
                  Success        => Success);

               if not Success then
                  Rose.Boot.Console.Put_Line
                    ("kernel: cannot restore process");
                  Return_Error (Params, Rose.Invocation.Request_Error);
                  return;
               end if;

               Rose.Boot.Console.Put ("kernel: restored process ");
               Rose.Kernel.Processes.Debug.Put (Pid);
               Rose.Boot.Console.New_Line;

               Params.all :=
                 (Control =>
                    (Flags          =>
                         (Rose.Invocation.Reply => True,
                          others                => False),
                     others         => <>),
                  others  => <>);

               Rose.Invocation.Send_Object_Id
                 (Params.all, To_Object_Id (Pid));
               Rose.Invocation.Send_Cap
                 (Params => Params.all,
                  Cap    =>
                    New_Cap (Current_Process_Id, Process_Interface_Cap (Pid)));
               Rose.Invocation.Send_Cap
                 (Params => Params.all,
                  Cap    =>
                    New_Cap (Current_Process_Id, Start_Process_Cap (Pid)));
               Rose.Invocation.Send_Cap
                 (Params => Params.all,
                  Cap    =>
                    New_Cap (Current_Process_Id, Initial_Cap (Pid)));
               Rose.Invocation.Send_Cap
                 (Params => Params.all,
                  Cap    =>
                    New_Cap (Current_Process_Id, Command_Endpoint_Cap (Pid)));

               Set_Current_State (Current_Process_Id, Ready);
            end;

         when Resume_Process_Endpoint =>
            declare
               use Rose.Kernel.Processes;
               Success : Boolean;
            begin
               Rose.Kernel.Processes.Snapshot.Resume_Restored_Process
                 (To_Process_Id
                    (Rose.Invocation.Get_Object_Id (Params.all, 0)),
                  Success);
               if not Success then
                  Return_Error (Params, Rose.Invocation.Request_Error);
                  return;
               end if;
               Params.Control.Flags :=
                 (Rose.Invocation.Reply => True, others => False);
               Set_Current_State (Current_Process_Id, Ready);
            end;

         when Restore_Interrupt_Endpoint =>
            declare
               use Rose.Kernel.Processes;
               use type Rose.Words.Word_32;
               Success : Boolean;
            begin
               if Params.Data (0) > 63 then
                  Return_Error (Params, Rose.Invocation.Request_Error);
                  return;
               end if;

               Rose.Kernel.Processes.Snapshot.Restore_Interrupt
                 (Interrupt =>
                    Rose.Arch.Interrupts.Interrupt_Vector (Params.Data (0)),
                  Object    => Rose.Invocation.Get_Object_Id (Params.all, 1),
                  Cap       => Rose.Capabilities.Capability (Params.Data (3)),
                  Success   => Success);
               if not Success then
                  Rose.Boot.Console.Put_Line
                    ("kernel: cannot restore interrupt");
                  Return_Error (Params, Rose.Invocation.Request_Error);
                  return;
               end if;
               Params.Control.Flags :=
                 (Rose.Invocation.Reply => True, others => False);
               Set_Current_State (Current_Process_Id, Ready);
            end;

         when Restart_Endpoint =>
            Rose.Boot.Console.Put_Line ("kernel: restarting");
            Rose.Arch.Restart;

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
