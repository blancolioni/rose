with System;

with Rose.Addresses;                   use Rose.Addresses;
with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;                       use Rose.Words;

package Rose.Boot.Console is

   procedure Init_Boot_Console;
   procedure Enable_Serial_Port;
   procedure Enable_Display;
   procedure Detach;

   procedure Clear;

   procedure Put (Text : String);
   procedure Put (Ch : Character);
   procedure Put (Item : Word_16);
   procedure Put (Item : Word_32);
   procedure Put (Addr : Physical_Address);
   procedure Put (Addr : Virtual_Address);
   procedure Put (Addr : System.Address);
   procedure Put (Object : Rose.Objects.Object_Id);
   procedure Put (Pid : Rose.Objects.Process_Id);
   procedure Put (Cap : Rose.Capabilities.Capability);
   procedure Put (X : Word_8);

   procedure Show_Stack (EIP, CS, PSW, ESP, SS : Word_32);
   pragma Export (C, Show_Stack, "debug_show_stack");

   procedure Put_Flag (Name   : String;
                       Value  : Boolean);

   procedure Put_Line (Text : String);

   procedure New_Line;

   procedure Move_To (Row, Column : Positive);
   function Current_Line return Positive;
   function Current_Column return Positive;

   procedure Handle_Keyboard
     (Scan_Code : Rose.Words.Word_32);
   pragma Export (C, Handle_Keyboard, "boot_console_kbd");

   procedure Disable_Console;

   procedure Status_Line
     (Current_Pid   : Rose.Objects.Process_Id;
      Current_Ticks : Rose.Words.Word;
      Page_Faults   : Natural);

private

   Terminal_Line, Terminal_Column : Natural := 0;

   function Current_Line return Positive is (Terminal_Line + 1);
   function Current_Column return Positive is (Terminal_Column + 1);

end Rose.Boot.Console;
