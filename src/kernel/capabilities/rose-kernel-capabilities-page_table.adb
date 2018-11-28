with Rose.Words;

with Rose.Kernel.Processes;

package body Rose.Kernel.Capabilities.Page_Table is

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
               use type Rose.Words.Word;
               use Rose.Invocation;

               Pid    : constant Rose.Kernel.Processes.Process_Id :=
                          Rose.Kernel.Processes.To_Process_Id
                            (Get_Object_Id (Params.all, 0));
               Page_Index : constant Parameter_Word_Index :=
                              (if Object_Fits_In_Word
                               then 1 else 2);
               Physical_Page : constant Physical_Page_Address :=
                                 Rose.Addresses.Physical_Page_Address
                                   (Params.Data (Page_Index + 0));
               Virtual_Page  : constant Virtual_Page_Address :=
                                 Virtual_Page_Address
                                   (Params.Data (Page_Index + 1));
               Permissions   : constant Rose.Words.Word :=
                                 (Params.Data (Page_Index + 2));
               Readable      : constant Boolean :=
                                 (Permissions and 1) /= 0;
               Writable      : constant Boolean :=
                                 (Permissions and 2) /= 0;
               Executable    : constant Boolean :=
                                 (Permissions and 4) /= 0;

            begin
               Rose.Kernel.Processes.Map_Page
                 (Pid           => Pid,
                  Virtual_Page  => Virtual_Page,
                  Physical_Page => Physical_Page,
                  Readable      => Readable,
                  Writable      => Writable,
                  Executable    => Executable,
                  User          => True);
            end;

            Params.Control :=
              (Flags => (Rose.Invocation.Reply => True, others => False),
               others => <>);

            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when others =>
            null;
      end case;

   end Handle;

end Rose.Kernel.Capabilities.Page_Table;
