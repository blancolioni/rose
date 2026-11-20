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

   ------------------
   -- Append_Batch --
   ------------------

   procedure Append_Batch
     (Pages : System.Address;
      Count : Positive;
      First : out Rose.Words.Word_32;
      Wrap  : out Rose.Words.Word_32)
   is
      use System.Storage_Elements;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send
        (Params,
         Rose.Interfaces.Checkpoints.Page_Log.Client.Get_Append_Batch_Cap
           (Client));
      Rose.System_Calls.Send_Buffer
        (Params, Storage_Count (Count) * Rose.Limits.Page_Size,
         Pages, False);
      Rose.System_Calls.Receive_Words (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         First := 0;
         Wrap := 0;
      else
         First := Rose.Words.Word_32 (Params.Data (0));
         Wrap := Rose.Words.Word_32 (Params.Data (1));
      end if;
   end Append_Batch;

   -------------------
   -- Discard_Pages --
   -------------------

   procedure Discard_Pages (From : Rose.Words.Word_32) is
   begin
      Rose.Interfaces.Checkpoints.Page_Log.Client.Discard_Pages
        (Client, Rose.Words.Word (From));
   end Discard_Pages;

   ----------------
   -- Entry_Last --
   ----------------

   function Entry_Last
     (Header : Rose.Words.Word_32)
      return Rose.Words.Word_32
   is
   begin
      return Rose.Words.Word_32
        (Rose.Interfaces.Checkpoints.Page_Log.Client.Entry_Last
           (Client, Rose.Words.Word (Header)));
   end Entry_Last;

   ---------------------
   -- Fail_Checkpoint --
   ---------------------

   procedure Fail_Checkpoint is
   begin
      Rose.Interfaces.Checkpoints.Page_Log.Client.Fail_Checkpoint (Client);
   end Fail_Checkpoint;

   -----------------
   -- Free_Before --
   -----------------

   procedure Free_Before (Log_Page : Rose.Words.Word_32) is
   begin
      Rose.Interfaces.Checkpoints.Page_Log.Client.Free_Before
        (Client, Rose.Words.Word (Log_Page));
   end Free_Before;

   ---------------
   -- Get_Space --
   ---------------

   procedure Get_Space
     (First : out Rose.Words.Word_32;
      Open  : out Rose.Words.Word_32;
      Last  : out Rose.Words.Word_32)
   is
      First_Word, Open_Word, Last_Word : Rose.Words.Word;
   begin
      Rose.Interfaces.Checkpoints.Page_Log.Client.Get_Space
        (Client, First_Word, Open_Word, Last_Word);
      First := Rose.Words.Word_32 (First_Word);
      Open := Rose.Words.Word_32 (Open_Word);
      Last := Rose.Words.Word_32 (Last_Word);
   end Get_Space;

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
