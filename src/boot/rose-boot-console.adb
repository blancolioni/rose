with Ada.Unchecked_Conversion;

with Rose.Arch;
with Rose.Version;

with Rose.Devices.Console;              use Rose.Devices.Console;
with Rose.Kernel.Clock;

package body Rose.Boot.Console is

   Status_Background            : constant := 16#7000#;
   Status_Foreground            : constant := 16#0800#;

   Serial_Port                  : constant := 16#03F8#;

   Current_Colour               : constant Word_16 := 16#700#;
   Status_Chars                 : String (1 .. 80);
   Status_Colours               : array (1 .. 80) of Word_16 :=
                                    (others => 16#0700#);
   Status_Memory                : array (1 .. 80) of Word_16;
   for Status_Memory'Address use System'To_Address (16#F00B_8000#);
   pragma Import (Ada, Status_Memory);

   Enabled             : Boolean := True;
   Serial_Port_Enabled : constant Boolean := True;
   Display_Enabled     : Boolean := True;

   --  use the known boot virtual address that points to the console
   Console_Memory : Console_Memory_Array;
   for Console_Memory'Address use
      System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address
           (Console_Start));

   Serial_Start_Of_Line : Boolean := True;

   procedure Put_Serial_Port (Ch : Character);
   procedure Put_Serial_Time_Stamp;

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

   procedure Enable_Serial_Port;

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

   ---------------------
   -- Disable_Display --
   ---------------------

   procedure Disable_Display is
   begin
      Display_Enabled := False;
   end Disable_Display;

   --------------------
   -- Enable_Display --
   --------------------

   procedure Enable_Display is
   begin
      Display_Enabled := True;
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

      Status_Line ("starting ...", 0, 0, 0, 0, 0, 0);

      Enable_Serial_Port;

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
         if Serial_Port_Enabled then
            Put_Serial_Port (Character'Val (10));
         end if;
         if Display_Enabled then
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
         if Serial_Port_Enabled then
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
         end if;
         if Display_Enabled then
            Console_Memory (Offset) := Current_Colour + Character'Pos (Ch);
            Update_Cursor_Position (Offset + 1);
         end if;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (X : Natural) is
      Buffer : String (1 .. 16);
      Start  : Natural := Buffer'Last + 1;
      It     : Natural := X;
   begin
      if It = 0 then
         Put ('0');
      else
         while It > 0 loop
            exit when Start = 1;
            Start := Start - 1;
            Buffer (Start) := Character'Val (It mod 10 + 48);
            It := It / 10;
         end loop;
         Put (Buffer (Start .. Buffer'Last));
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

   procedure Put (Endpoint : Rose.Objects.Endpoint_Id) is
   begin
      Put (Rose.Objects.Object_Id (Endpoint));
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

   ---------
   -- Put --
   ---------

   procedure Put (Item : System.Storage_Elements.Storage_Array) is
      Offset : Word_16 := 0;
   begin
      for X of Item loop
         if Offset mod 16 = 0 then
            Put (Offset);
            Put (":");
         end if;
         Put (" ");
         Put (Word_8 (X));
         Offset := Offset + 1;
         if Offset mod 16 = 0 then
            New_Line;
         end if;
      end loop;

      if Offset mod 16 /= 0 then
         New_Line;
      end if;
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
      if Serial_Start_Of_Line then
         Serial_Start_Of_Line := False;
         Put_Serial_Time_Stamp;
      end if;

      loop
         Flag := Rose.Arch.Inb (Serial_Port + 5);
         exit when (Flag and 16#20#) /= 0;
      end loop;
      Rose.Arch.Outb (Serial_Port, Character'Pos (Ch));

      Serial_Start_Of_Line := Ch = Character'Val (10);

   end Put_Serial_Port;

   ---------------------------
   -- Put_Serial_Time_Stamp --
   ---------------------------

   procedure Put_Serial_Time_Stamp is
      S : String (1 .. 10);
      X : Word := Rose.Kernel.Clock.Current_Ticks;
   begin
      for Ch of reverse S loop
         Ch := Character'Val (X mod 10 + 48);
         X := X / 10;
      end loop;
      Put_Serial_Port ('[');
      for Ch of S loop
         Put_Serial_Port (Ch);
      end loop;
      Put_Serial_Port (']');
      Put_Serial_Port (' ');
   end Put_Serial_Time_Stamp;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Text   : String;
                         Update : Boolean)
   is
      Offset : Natural := Current_Offset;
   begin
      if Enabled then
         if Serial_Port_Enabled then
            for Ch of Text loop
               Put_Serial_Port (Ch);
            end loop;
         end if;
         if Display_Enabled then
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
      if Enabled and then Display_Enabled then
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
     (Current_Process : String;
      Current_Ticks   : Rose.Words.Word;
      Page_Faults     : Natural;
      Mem_Allocated   : Rose.Addresses.Physical_Bytes;
      Mem_Available   : Rose.Addresses.Physical_Bytes;
      Heap_Allocated  : Rose.Addresses.Physical_Bytes;
      Heap_Available  : Rose.Addresses.Physical_Bytes)
   is
      Time_Image  : String (1 .. 8);
      Acc         : Word_32 := Current_Ticks / 100;
      Fault_Acc   : Natural := Page_Faults;
      Heap        : String (1 .. 30) := (others => ' ');
      Heap_Count  : Natural := 0;

      procedure Heap_Add (Text : String);
      procedure Heap_Add (Num  : Natural);

      --------------
      -- Heap_Add --
      --------------

      procedure Heap_Add (Text : String) is
      begin
         for Ch of Text loop
            Heap_Count := Heap_Count + 1;
            exit when Heap_Count > Heap'Last;
            Heap (Heap_Count) := Ch;
         end loop;
      end Heap_Add;

      --------------
      -- Heap_Add --
      --------------

      procedure Heap_Add (Num  : Natural) is
         Image : String (1 .. 10);
         Start : Natural := Image'Last + 1;
         It    : Natural := Num;
      begin
         if Num = 0 then
            Heap_Add ("0");
         else
            while It /= 0 loop
               Start := Start - 1;
               Image (Start) := Character'Val (It mod 10 + 48);
               It := It / 10;
            end loop;
            Heap_Add (Image (Start .. Image'Last));
         end if;
      end Heap_Add;

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

      Status_Chars (72 .. 79) := Time_Image;

      declare
         Name_Index : Positive := 48;
      begin
         for Ch of Current_Process loop
            Name_Index := Name_Index + 1;
            exit when Name_Index > 62;
            Status_Chars (Name_Index) := Ch;
         end loop;
         while Name_Index < 62 loop
            Name_Index := Name_Index + 1;
            Status_Chars (Name_Index) := ' ';
         end loop;
      end;

      for Index in reverse 64 .. 70 loop
         if Fault_Acc = 0 and then Index < 70 then
            Status_Chars (Index) := ' ';
         else
            Status_Chars (Index) := Character'Val (48 + Fault_Acc mod 10);
            Fault_Acc := Fault_Acc / 10;
         end if;
      end loop;

      Heap_Add ("kmem: ");
      Heap_Add (Natural (Heap_Allocated) / 1024);
      Heap_Add ("K");
      Heap_Add ("/");
      Heap_Add (Natural (Heap_Allocated + Heap_Available) / 1024);
      Heap_Add ("K");

      Heap_Add (" mem: ");
      Heap_Add (Natural (Mem_Allocated) / 1024);
      Heap_Add ("K");
      Heap_Add ("/");
      Heap_Add (Natural (Mem_Allocated + Mem_Available) / 1024 / 1024);
      Heap_Add ("M");

      Status_Chars (17 .. 16 + Heap'Length) := Heap;

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
