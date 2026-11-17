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

   procedure Read
     (Log_Page : Rose.Words.Word_32;
      Page     : System.Address);
   --  Read log page Log_Page into the page at Page.

   procedure Fail_Entry;
   --  Tell the log server to leave the open entry out of the log when it
   --  commits.

end Mem.Page_Log;
