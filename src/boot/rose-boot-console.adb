with System.Storage_Elements;           use System.Storage_Elements;

with Ada.Unchecked_Conversion;

with Rose.Arch;
with Rose.Version;

with Rose.Devices.Console;              use Rose.Devices.Console;

package body Rose.Boot.Console is

   Status_Background            : constant := 16#7000#;
   Status_Foreground            : constant := 16#0800#;

   Serial_Port                  : constant := 16#03F8#;

   Current_Colour               : constant Word_16 := 16#700#;
   Status_Chars                 : String (1 .. 80);
   Status_Colours               : array (1 .. 80) of Word_16 :=
                                    (others => 16#0700#);
   Status_Memory                : array (1 .. 80) of Word_16;
   for Status_Memory'Address use System'To_Address (16#C00B_8000#);
   pragma Import (Ada, Status_Memory);

   Enabled          : Boolean := True;
   Serial_Port_Mode : Boolean := False;

   --  use the known boot virtual address that points to the console
   Console_Memory : Console_Memory_Array;
   for Console_Memory'Address use
      System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address
           (Console_Start));

   procedure Put_Serial_Port (Ch : Character);

   --  Current_Offset: current index into Console_Memory
   function Current_Offset return Natural;

   --  Update then cursor position based on the given offset
   --  into the console memory
   procedure Update_Cursor_Position (Offset : Natural);

   --  Scroll the cursor position onto screen if necessary
   procedure Scroll;

   procedure Update_Cursor;

   function Hex_Digit (N : Word_32) return Character;

   procedure Put_String (Text   : String;
                         Update : Boolean);

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Console_Memory := (others => 16#0720#);
      Terminal_Line := 0;
      Terminal_Column := 0;
      Update_Cursor;
   end Clear;

   --------------------
   -- Current_Offset --
   --------------------

   function Current_Offset return Natural is
   begin
      return Terminal_Line * Num_Columns + Terminal_Column;
   end Current_Offset;

   ------------
   -- Detach --
   ------------

   procedure Detach is
   begin
      Put_Line ("boot-console: detaching");
      Enabled := False;
   end Detach;

   ---------------------
   -- Disable_Console --
   ---------------------

   procedure Disable_Console is
   begin
      null;    --  Enabled := False;
   end Disable_Console;

   --------------------
   -- Enable_Display --
   --------------------

   procedure Enable_Display is
   begin
      Serial_Port_Mode := False;
   end Enable_Display;

   ------------------------
   -- Enable_Serial_Port --
   ------------------------

   procedure Enable_Serial_Port is
   begin
      Rose.Arch.Outb (Serial_Port + 1, 16#00#);
      Rose.Arch.Outb (Serial_Port + 3, 16#80#);
      Rose.Arch.Outb (Serial_Port + 0, 16#01#);
      Rose.Arch.Outb (Serial_Port + 1, 16#00#);
      Rose.Arch.Outb (Serial_Port + 3, 16#03#);
      Rose.Arch.Outb (Serial_Port + 2, 16#C7#);
      Rose.Arch.Outb (Serial_Port + 1, 16#00#);
      Serial_Port_Mode := True;
   end Enable_Serial_Port;

   ---------------------
   -- Handle_Keyboard --
   ---------------------

   procedure Handle_Keyboard
     (Scan_Code : Rose.Words.Word_32)
   is
   begin
      Put (Character'Val (Scan_Code + 32));
   end Handle_Keyboard;

   ---------------
   -- Hex_Digit --
   ---------------

   function Hex_Digit (N : Word_32) return Character is
   begin
      if N < 10 then
         return Character'Val (N + 48);
      else
         return Character'Val (N + 55);
      end if;
   end Hex_Digit;

   ------------------------
   -- Initialise_Console --
   ------------------------

   procedure Init_Boot_Console is
   begin
      Terminal_Line := 0;
      Terminal_Column := 0;
      Clear;
      Console.Put_Line (Rose.Version.Full_Name);
      Status_Chars :=
        "Rose -'-,-{@        "
        & "                    "
        & "                    "
        & "                    ";
      Status_Colours :=
        (1 .. 4 => 16#0500# + Status_Background,
         6 .. 11 => 16#0200# + Status_Background,
         12 => 16#0400# + Status_Background,
         others => Status_Foreground + Status_Background);

      Status_Line (0, 0, 0);

   end Init_Boot_Console;

   -------------
   -- Move_To --
   -------------

   procedure Move_To (Row, Column : Positive) is
   begin
      Terminal_Line := Row - 1;
      Terminal_Column := Column - 1;
      Update_Cursor;
   end Move_To;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      if Enabled then
         if Serial_Port_Mode then
            Put_Serial_Port (Character'Val (10));
         else
            Terminal_Line := Terminal_Line + 1;
            Terminal_Column := 0;
            Scroll;
         end if;
      end if;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      Put_String (Text, True);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Ch : Character) is
      Offset : constant Natural := Current_Offset;
   begin
      if Enabled then
         if Serial_Port_Mode then
            if Character'Pos (Ch) in 32 .. 127 then
               Put_Serial_Port (Ch);
            else
               Put_Serial_Port ('[');
               Put_Serial_Port
                 (Hex_Digit (Character'Pos (Ch) / 256 mod 256));
               Put_Serial_Port
                 (Hex_Digit (Character'Pos (Ch) mod 256));
               Put_Serial_Port (']');
            end if;
         else
            Console_Memory (Offset) := Current_Colour + Character'Pos (Ch);
            Update_Cursor_Position (Offset + 1);
         end if;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (X : Word_8) is
   begin
      Put (Hex_Digit (Word_32 (X / 16)));
      Put (Hex_Digit (Word_32 (X mod 16)));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Word_16) is
   begin
      Put (Word_8 (Item / 256));
      Put (Word_8 (Item mod 256));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Word_32) is
      N : Word_32 := Item;
      T : Word_32;
      Buffer : String (1 .. 9);
   begin
      for I in reverse 1 .. 9 loop
         if I = 5 then
            Buffer (I) := '_';
         else
            T := N mod 16;
            N := N / 16;
            if T > 9 then
               Buffer (I) := Character'Val (T - 10 + 65);
            else
               Buffer (I) := Character'Val (48 + T);
            end if;
         end if;
      end loop;
      Put (Buffer);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Pid : Rose.Objects.Process_Id) is
   begin
      Put ("pid-");
      Put (Rose.Words.Word_8 (Pid));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Cap : Rose.Capabilities.Capability) is
   begin
      Put ("cap-");
      Put (Rose.Words.Word_8 (Cap));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Object : Rose.Objects.Object_Id) is
      use Rose.Objects;
   begin
      Put (Word_32 (Object / 2 ** 32));
      Put ("_");
      Put (Word_32 (Object mod 2 ** 32));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Addr : System.Address) is
      function To_Word is
         new Ada.Unchecked_Conversion (System.Address, Word);
   begin
      Put (To_Word (Addr));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Addr : Physical_Address) is
   begin
      Put (Word_32 (Addr));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Addr : Virtual_Address) is
      N1, N2, N3 : Word_32;
      Buffer : String (1 .. 12);
   begin
      N3 := Word_32 (Addr mod 4096);
      N2 := Word_32 (Addr / 4096 mod 1024);
      N1 := Word_32 (Addr / 4096 / 1024);

      Buffer (1) := Hex_Digit (N1 / 256);
      Buffer (2) := Hex_Digit (N1 / 16 mod 16);
      Buffer (3) := Hex_Digit (N1 mod 16);

      Buffer (4) := ':';

      Buffer (5) := Hex_Digit (N2 / 256);
      Buffer (6) := Hex_Digit (N2 / 16 mod 16);
      Buffer (7) := Hex_Digit (N2 mod 16);

      Buffer (8) := ':';

      Buffer (9)  := Hex_Digit (N3 / 4096);
      Buffer (10) := Hex_Digit (N3 mod 4096 / 256);
      Buffer (11) := Hex_Digit (N3 mod 256 / 16);
      Buffer (12) := Hex_Digit (N3 mod 16);

      Put (Buffer);
   end Put;

   --------------
   -- Put_Flag --
   --------------

   procedure Put_Flag (Name   : String;
                       Value  : Boolean)
   is
   begin
      Put (Name);
      Put (": ");
      Put (if Value then "yes" else "no");
      New_Line;
   end Put_Flag;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
   begin
      Put_String (Text, False);
      New_Line;
   end Put_Line;

   ---------------------
   -- Put_Serial_Port --
   ---------------------

   procedure Put_Serial_Port (Ch : Character) is
      Flag : Word_8;
   begin
      loop
         Flag := Rose.Arch.Inb (Serial_Port + 5);
         exit when (Flag and 16#20#) /= 0;
      end loop;
      Rose.Arch.Outb (Serial_Port, Character'Pos (Ch));
   end Put_Serial_Port;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Text   : String;
                         Update : Boolean)
   is
      Offset : Natural := Current_Offset;
   begin
      if Enabled then
         if Serial_Port_Mode then
            for Ch of Text loop
               Put (Ch);
            end loop;
         else
            for I in Text'Range loop
               Console_Memory (Offset) :=
                 Current_Colour + Character'Pos (Text (I));
               Offset := Offset + 1;
            end loop;
            if Update then
               Update_Cursor_Position (Offset);
            end if;
         end if;
      end if;
   end Put_String;

   ------------
   -- Scroll --
   ------------

   procedure Scroll is
   begin
      if Enabled and then not Serial_Port_Mode then
         while Terminal_Line >= Num_Lines loop
            Console_Memory (0 .. (Num_Lines - 1) * Num_Columns - 1) :=
              Console_Memory (Num_Columns .. Num_Lines * Num_Columns - 1);
            Console_Memory ((Num_Lines - 1) * Num_Columns ..
                              Num_Lines * Num_Columns - 1) :=
                (others => 16#0720#);

            Terminal_Line := Terminal_Line - 1;
         end loop;
         Update_Cursor;
      end if;
   end Scroll;

   ----------------
   -- Show_Stack --
   ----------------

   procedure Show_Stack (EIP, CS, PSW, ESP, SS : Word_32) is
   begin
      Put ("EIP   ");
      Put (EIP);
      New_Line;
      Put ("CS    ");
      Put (CS);
      New_Line;
      Put ("PSW   ");
      Put (PSW);
      New_Line;
      Put ("ESP   ");
      Put (ESP);
      New_Line;
      Put ("SS    ");
      Put (SS);
      New_Line;
      if False then
         declare
            Addr : constant System.Address :=
                     System'To_Address (EIP);
            Code : System.Storage_Elements.Storage_Array (1 .. 16);
            for Code'Address use Addr;
            pragma Import (Ada, Code);
         begin
            Put ("Code:");
            for Element of Code loop
               Put (" ");
               Put (Rose.Words.Word_8 (Element));
            end loop;
            New_Line;
         end;
      end if;
   end Show_Stack;

   -----------------
   -- Status_Line --
   -----------------

   procedure Status_Line
     (Current_Pid   : Rose.Objects.Process_Id;
      Current_Ticks : Rose.Words.Word;
      Page_Faults   : Natural)
   is
      Time_Image  : String (1 .. 8);
      Acc         : Word_32 := Current_Ticks / 100;
      Pid_Acc     : Natural := Natural (Current_Pid);
      Fault_Acc   : Natural := Page_Faults;
   begin
      Time_Image (8) := Character'Val (Acc mod 10 + 48);
      Acc := Acc / 10;
      Time_Image (7) := Character'Val (Acc mod 6 + 48);
      Acc := Acc / 6;
      Time_Image (6) := ':';
      Time_Image (5) := Character'Val (Acc mod 10 + 48);
      Acc := Acc / 10;
      Time_Image (4) := Character'Val (Acc mod 6 + 48);
      Acc := Acc / 6;
      Time_Image (3) := ':';
      Time_Image (2) := Character'Val (Acc mod 10 + 48);
      Acc := Acc / 10;
      Time_Image (1) := Character'Val (Acc mod 10 + 48);

      Status_Chars (70 .. 77) := Time_Image;

      for Pid_Index in reverse 62 .. 66 loop
         if Pid_Acc = 0 and then Pid_Index < 66 then
            Status_Chars (Pid_Index) := ' ';
         else
            Status_Chars (Pid_Index) := Character'Val (48 + Pid_Acc mod 10);
            Pid_Acc := Pid_Acc / 10;
         end if;
      end loop;

      for Index in reverse 56 .. 60 loop
         if Fault_Acc = 0 and then Index < 60 then
            Status_Chars (Index) := ' ';
         else
            Status_Chars (Index) := Character'Val (48 + Fault_Acc mod 10);
            Fault_Acc := Fault_Acc / 10;
         end if;
      end loop;

      for I in Status_Chars'Range loop
         Status_Memory (I) :=
           Status_Colours (I) + Character'Pos (Status_Chars (I));
      end loop;
   end Status_Line;

   -------------------
   -- Update_Cursor --
   -------------------

   procedure Update_Cursor is
      Position    : constant Natural :=
        (Terminal_Line + 1) * Num_Columns + Terminal_Column;
   begin
      Rose.Arch.Outb (16#03D4#, 16#0E#);
      Rose.Arch.Outb (16#03D5#, Word_8 (Position / 256 mod 256));
      Rose.Arch.Outb (16#03D4#, 16#0F#);
      Rose.Arch.Outb (16#03D5#, Word_8 (Position mod 256));
   end Update_Cursor;

   ----------------------------
   -- Update_Cursor_Position --
   ----------------------------

   procedure Update_Cursor_Position (Offset : Natural) is
   begin
      Terminal_Line := Offset / Num_Columns;
      Terminal_Column := Offset mod Num_Columns;
      Scroll;
   end Update_Cursor_Position;

end Rose.Boot.Console;
