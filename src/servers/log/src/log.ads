--  Root package for Log server

with Rose.Capabilities;
with Rose.Words;

package Log is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;

   Console_Cap         : Rose.Capabilities.Capability;
   Start_Log_Cap       : Rose.Capabilities.Capability;

   Log_Page_Size : constant := 2 ** 12;
   Log_Page_Bits : constant := Log_Page_Size * 8;

   Log_Version   : constant Rose.Words.Word_32 := 16#0001#;
   type Log_Location_Id is new Rose.Words.Word_64;

end Log;
