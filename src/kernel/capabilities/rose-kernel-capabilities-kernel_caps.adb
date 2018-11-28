with System.Storage_Elements;

with Rose.Words;

with Rose.Kernel.Processes;
with Rose.Kernel.Panic;

with Rose.Boot.Console;

package body Rose.Kernel.Capabilities.Kernel_Caps is

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

         when others =>
            Rose.Kernel.Panic.Panic
              ("bad kernel cap endpoint");

      end case;
   end Handle;

end Rose.Kernel.Capabilities.Kernel_Caps;
