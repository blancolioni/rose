--  Root package for Log server

with Rose.Capabilities;
with Rose.Words;

package Log is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;

   Console_Cap         : Rose.Capabilities.Capability;
   Start_Log_Cap       : Rose.Capabilities.Capability;
   Store_Committed_Cap : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   --  The store's Storage_Checkpoint.Committed, which is called after
   --  each entry is committed (ADR 0038, section 6).

   Log_Page_Size : constant := 2 ** 12;
   Log_Page_Bits : constant := Log_Page_Size * 8;

   Log_Version   : constant Rose.Words.Word_32 := 16#0003#;
   --  Version 2 added the generation to the log and entry headers, and
   --  version 3 the sequence number to the entry header, so that the
   --  log can wrap around (#951).
   type Log_Location_Id is new Rose.Words.Word_64;

end Log;
