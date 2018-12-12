with Rose.Boot.Console;

with Rose.Limits;
with Rose.Words;

with Rose.Kernel.Clock;
with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Capabilities.Page_Table is

   procedure Map_Process_Page
     (Params : Rose.Invocation.Invocation_Access;
      Pid    : Rose.Kernel.Processes.Process_Id;
      Start  : Rose.Invocation.Parameter_Word_Index);

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
   begin
      case Cap.Header.Endpoint is
         when Map_Page =>
            declare
               use Rose.Invocation;
               Pid         : constant Rose.Kernel.Processes.Process_Id :=
                               Rose.Kernel.Processes.To_Process_Id
                                 (Rose.Invocation.Get_Object_Id
                                    (Params.all, 0));
               Start_Index : constant Parameter_Word_Index :=
                               (if Object_Fits_In_Word
                                then 1 else 2);
            begin
               Map_Process_Page (Params, Pid, Start_Index);
            end;

         when Unmap_Page =>
            Rose.Boot.Console.Put_Line
              ("kernel: unmap page not implemented");

         when Set_Page =>
            declare
               use type Rose.Invocation.Parameter_Word_Index;
               Pid : constant Rose.Kernel.Processes.Process_Id :=
                       Rose.Kernel.Processes.Current_Process_Id;
            begin
               if Params.Control.Last_Sent_Word = 0 then
                  Rose.Kernel.Processes.Unmap_Page
                    (Pid          => Pid,
                     Virtual_Page =>
                       Rose.Addresses.Virtual_Page_Address
                         (Params.Data (0)));
               else
                  Map_Process_Page (Params, Pid, 0);
               end if;
            end;

         when others =>
            Rose.Boot.Console.Put
              ("kernel: page table: unknown endpoint: ");
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Cap.Header.Endpoint));
            Rose.Boot.Console.New_Line;
      end case;

   end Handle;

   ----------------------
   -- Map_Process_Page --
   ----------------------

   procedure Map_Process_Page
     (Params : Rose.Invocation.Invocation_Access;
      Pid    : Rose.Kernel.Processes.Process_Id;
      Start  : Rose.Invocation.Parameter_Word_Index)
   is
      use type Rose.Words.Word;
      use Rose.Invocation;

      Page_Index      : constant Parameter_Word_Index := Start;
      Physical_Page   : constant Physical_Page_Address :=
                          Rose.Addresses.Physical_Page_Address
                            (Params.Data (Page_Index + 0));
      Virtual_Page    : constant Virtual_Page_Address :=
                          Virtual_Page_Address
                            (Params.Data (Page_Index + 1));
      Permissions     : constant Rose.Words.Word :=
                          Params.Data (Page_Index + 2);
      Available_Pages : constant Rose.Words.Word :=
                          Params.Data (Page_Index + 3);
      Allocated_Pages : constant Rose.Words.Word :=
                          Params.Data (Page_Index + 4);
      Readable        : constant Boolean :=
                          (Permissions and 1) /= 0;
      Writable        : constant Boolean :=
                          (Permissions and 2) /= 0;
      Executable      : constant Boolean :=
                          (Permissions and 4) /= 0;

   begin
      Rose.Boot.Console.Put ("map-page: ");
      Rose.Kernel.Processes.Debug.Put (Pid);
      Rose.Boot.Console.Put (" physical=");
      Rose.Boot.Console.Put (Rose.Words.Word (Physical_Page) * 4096);
      Rose.Boot.Console.Put (" virtual=");
      Rose.Boot.Console.Put (Rose.Words.Word (Virtual_Page) * 4096);
      Rose.Boot.Console.New_Line;

      Rose.Kernel.Processes.Map_Page
        (Pid           => Pid,
         Virtual_Page  => Virtual_Page,
         Physical_Page => Physical_Page,
         Readable      => Readable,
         Writable      => Writable,
         Executable    => Executable,
         User          => True);
      Rose.Kernel.Clock.Update_Mem
        (Allocated =>
           Physical_Bytes (Allocated_Pages * Rose.Limits.Page_Size),
         Available =>
           Physical_Bytes (Available_Pages * Rose.Limits.Page_Size));
      Params.Control :=
        (Flags  => (Rose.Invocation.Reply => True, others => False),
         others => <>);

      Rose.Kernel.Processes.Set_Current_State
        (Rose.Kernel.Processes.Current_Process_Id,
         Rose.Kernel.Processes.Ready);

   end Map_Process_Page;

end Rose.Kernel.Capabilities.Page_Table;
