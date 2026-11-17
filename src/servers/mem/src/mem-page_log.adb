with System.Storage_Elements;

with Rose.Invocation;
with Rose.Limits;
with Rose.System_Calls;

with Rose.Interfaces.Checkpoints.Page_Log.Client;

package body Mem.Page_Log is

   use type Rose.Capabilities.Capability;

   Page_Log_Cap  : Rose.Capabilities.Capability := 0;
   Client        : Rose.Interfaces.Checkpoints.Page_Log.Client.Page_Log_Client;
   Client_Open   : Boolean := False;

   ------------
   -- Append --
   ------------

   function Append (Page : System.Address) return Rose.Words.Word_32 is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send
        (Params,
         Rose.Interfaces.Checkpoints.Page_Log.Client.Get_Append_Page_Cap
           (Client));
      Rose.System_Calls.Send_Buffer (Params, Rose.Limits.Page_Size,
                                     Page, False);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         return 0;
      end if;

      return Rose.Words.Word_32 (Params.Data (0));
   end Append;

   ----------------
   -- Fail_Entry --
   ----------------

   procedure Fail_Entry is
   begin
      Rose.Interfaces.Checkpoints.Page_Log.Client.Fail_Entry (Client);
   end Fail_Entry;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available return Boolean is
   begin
      if not Client_Open and then Page_Log_Cap /= 0 then
         Rose.Interfaces.Checkpoints.Page_Log.Client.Open
           (Client, Page_Log_Cap);
         Client_Open := True;
      end if;
      return Client_Open;
   end Is_Available;

   ----------
   -- Read --
   ----------

   procedure Read
     (Log_Page : Rose.Words.Word_32;
      Page     : System.Address)
   is
      Data : System.Storage_Elements.Storage_Array
        (1 .. Rose.Limits.Page_Size)
        with Import, Address => Page;
   begin
      Rose.Interfaces.Checkpoints.Page_Log.Client.Read_Page
        (Client, Rose.Words.Word (Log_Page), Data);
   end Read;

   -------------
   -- Set_Cap --
   -------------

   procedure Set_Cap (Cap : Rose.Capabilities.Capability) is
   begin
      Page_Log_Cap := Cap;
   end Set_Cap;

end Mem.Page_Log;
