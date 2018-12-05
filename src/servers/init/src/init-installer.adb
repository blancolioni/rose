with System.Storage_Elements;

with Rose.Interfaces.Stream_Reader.Client;

with Rose.Invocation;
with Rose.System_Calls;

with Init.Calls;

package body Init.Installer is

   ----------------------
   -- Launch_With_Caps --
   ----------------------

   procedure Launch_With_Caps
     (Create_Cap    : Rose.Capabilities.Capability;
      Launch_Cap    : Rose.Capabilities.Capability;
      Cap_Stream    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability)
   is
      use Rose.Interfaces.Stream_Reader.Client;
      use System.Storage_Elements;
      Client      : Stream_Reader_Client;
      Last        : Storage_Count;
      Storage     : Storage_Array (1 .. 16);
      Layout      : Init.Calls.Array_Of_Words (1 .. 4);
      pragma Import (Ada, Layout);
      for Layout'Address use Storage'Address;
      Launch_Caps : Rose.System_Calls.Sent_Caps_Array (1 .. 10);
      Cap_Count   : Natural := 0;
   begin
      Open_Cap_Set (Client, Cap_Stream);

      loop
         Read (Client, Storage, Last);
         exit when Has_Error or else Last = 0;
         Cap_Count := Cap_Count + 1;
         Launch_Caps (Cap_Count) :=
           Init.Calls.Call (Create_Cap, Layout);
      end loop;

      if Has_Error then
         return;
      end if;

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Launch_Cap);
         Rose.System_Calls.Send_Cap (Params, Binary_Stream);
         for Cap of Launch_Caps (1 .. Cap_Count) loop
            Rose.System_Calls.Send_Cap (Params, Cap);
         end loop;
         Rose.System_Calls.Invoke_Capability (Params);
      end;
   end Launch_With_Caps;

end Init.Installer;
