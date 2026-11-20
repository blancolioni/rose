with System;

with Rose.Capabilities;
with Rose.Words;

package Mem.Page_Log is

   --  The log server's Page_Log (ADR 0028), which init gives mem once the
   --  log server is running (Processes.Memory.Set_Page_Log). It is
   --  opened the first time it is used, rather than when it is given, so
   --  that mem doesn't wait for the log server while init does.

   procedure Set_Cap (Cap : Rose.Capabilities.Capability);

   function Is_Available return Boolean;
   --  Init has given mem the cap. Opens the client the first time.

   function Append (Page : System.Address) return Rose.Words.Word_32;
   --  Append the page at Page, which is page aligned, to the open entry,
   --  and return its log page, or 0 if the entry has failed. The page is
   --  sent from where it is, rather than copied into an invocation
   --  buffer.

   procedure Append_Batch
     (Pages : System.Address;
      Count : Positive;
      First : out Rose.Words.Word_32;
      Wrap  : out Rose.Words.Word_32);
   --  Append the Count pages at Pages, next to each other and page
   --  aligned, to the open entry in one call (Page_Log.Append_Batch,
   --  #974). They are sent from where they are. First is the log page
   --  of the first, or 0 if the batch wasn't appended, and Wrap is how
   --  many went before the log wrapped round to log page 1, or 0.

   procedure Read
     (Log_Page : Rose.Words.Word_32;
      Page     : System.Address);
   --  Read log page Log_Page into the page at Page.

   procedure Fail_Checkpoint;
   --  Tell the log server to drop the checkpoint's pages when it commits.
   --  The pages appended before the checkpoint stay in the open entry.

   procedure Discard_Pages (From : Rose.Words.Word_32);
   --  Between checkpoints, drop log page From of the open entry, and
   --  every page after it, after an append of a batch failed. From is
   --  the log page of the batch's first page, or 0 if that append failed.

end Mem.Page_Log;
