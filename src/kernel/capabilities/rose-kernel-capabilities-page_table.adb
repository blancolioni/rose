with Rose.Words;

with Rose.Kernel.Processes;

package body Rose.Kernel.Capabilities.Page_Table is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Generic_Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
   begin
      case Cap.Header.Endpoint is
         when Map_Page =>
            declare
               use type Rose.Words.Word;
               Process_Id    : constant Rose.Objects.Process_Id :=
                                 Rose.Objects.Process_Id
                                   (Params.Data (0));
               Physical_Page : constant Physical_Page_Address :=
                                 Rose.Addresses.Physical_Page_Address
                                   (Params.Data (1));
               Virtual_Page  : constant Virtual_Page_Address :=
                                 Virtual_Page_Address
                                   (Params.Data (2));
               Permissions   : constant Rose.Words.Word :=
                                 Params.Data (3);
               Readable      : constant Boolean :=
                                 (Permissions and 1) /= 0;
               Writable      : constant Boolean :=
                                 (Permissions and 2) /= 0;
               Executable    : constant Boolean :=
                                 (Permissions and 4) /= 0;

            begin
               Rose.Kernel.Processes.Map_Page
                 (Process       => Process_Id,
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
