with System;

with Rose.Boot.Console;

package body Rose.Multiboot is

   Report_Memory_Map : constant Boolean := False;
   Report_Modules    : constant Boolean := True;

   --  package APM_Table_Convert is
   --    new System.Address_To_Access_Conversions
   --      (Object => APM_Table_Access);

   --  package Convert is new  System.Address_To_Access_Conversions
   --    (Object => Memory_Map_Entry_Access);

   --  function Word_32_To_Entry_Access
   --    (Addr : Rose.Words.Word_32) return Memory_Map_Entry_Access;

   ----------------------------
   -- First_Memory_Map_Entry --
   ----------------------------

   function First_Memory_Map_Entry return Memory_Map_Entry_Access is (null);

   --  begin
   --     if not Info.Flags.BIOS_Memory_Map then
   --        return null;
   --     end if;

   --     if Info.Memory_Map.Addr = 0 then
   --        return null;
   --     end if;

   --     Rose.Boot.Console.Put ("first memory map entry: ");
   --     Rose.Boot.Console.Put (Info.Memory_Map.Addr);
   --     Rose.Boot.Console.New_Line;

   --     Rose.Boot.Console.Put ("memory map length: ");
   --     Rose.Boot.Console.Put (Info.Memory_Map.Length);
   --     Rose.Boot.Console.New_Line;

   --     declare
   --        use Rose.Boot.Console;
   --        Mem : constant Memory_Map_Entry_Access :=
   --          Word_32_To_Entry_Access (Info.Memory_Map.Addr - 4);
   --     begin
   --        Put ("   size: ");
   --        Put (Mem.Size);
   --        New_Line;
   --        Put ("   base: ");
   --        Put (Mem.Base_High);
   --        Put (":");
   --        Put (Mem.Base_Low);
   --        New_Line;
   --        Put ("   length: ");
   --        Put (Mem.Length_High);
   --        Put (":");
   --        Put (Mem.Length_Low);
   --        New_Line;
   --        Put ("   type: ");
   --        Put (if Mem.Sort /= Memory_Available
   --               then "reserved" else "available");
   --        New_Line;
   --        return Mem;
   --     end;
   --  end First_Memory_Map_Entry;

   procedure Load_Multiboot_Header is
      Local_Address : constant System.Address :=
        System'To_Address (Info_Address);
      Local_Header : Multiboot_Header_Array;
      for Local_Header'Address use Local_Address;
      pragma Import (Ada, Local_Header);
   begin
      Header := Local_Header;
   end Load_Multiboot_Header;

   -------------------
   -- Get_APM_Table --
   -------------------

   function Get_APM_Table return APM_Table_Access is (null);

   --     function To_APM_Table_Access is new Ada.Unchecked_Conversion
   --       (Source => APM_Table_Convert.Object_Pointer,
   --        Target => APM_Table_Access);
   --  begin
   --     if not Info.Flags.APM_Table then
   --        return null;
   --     end if;

   --     return To_APM_Table_Access
   --       (APM_Table_Convert.To_Pointer (Info.APM));
   --  end Get_APM_Table;

   -------------------------
   -- Get_Symbols_Variant --
   -------------------------

   function Get_Symbols_Variant return Symbols_Variant is (ELF);
   --  begin
   --     if Info.Flags.Symbol_Table
   --  and not Info.Flags.Section_Header_Table then
   --        return Aout;
   --     elsif not Info.Flags.Symbol_Table and
   --       Info.Flags.Section_Header_Table
   --     then
   --        return ELF;
   --     else
   --        return ELF;
   --     end if;
   --  end Get_Symbols_Variant;

   ---------------------------
   -- Next_Memory_Map_Entry --
   ---------------------------

   function Next_Memory_Map_Entry
     (Current : Memory_Map_Entry_Access)
      return Memory_Map_Entry_Access
   is (null);

   --     function To_Word_32 is new Ada.Unchecked_Conversion
   --       (Source => System.Address,
   --        Target => Rose.Words.Word_32);

   --     function To_Object_Pointer is new Ada.Unchecked_Conversion
   --       (Source => Memory_Map_Entry_Access,
   --        Target => Convert.Object_Pointer);

   --     Current_Addr : constant Rose.Words.Word_32 :=
   --                      To_Word_32
   --                        (Convert.To_Address
   --                           (To_Object_Pointer (Current)));
   --     Next_Addr    : Rose.Words.Word_32 := Rose.Words.Word_32'First;
   --  begin
   --     if  Current_Addr >=
   --  Info.Memory_Map.Addr + Info.Memory_Map.Length then
   --        return null;
   --     end if;

   --     Next_Addr := Current_Addr + Current.Size +
   --       (Rose.Words.Word_32'Size / System.Storage_Unit);

   --     return Word_32_To_Entry_Access (Next_Addr);
   --  end Next_Memory_Map_Entry;

   ---------------------------
   -- Scan_Kernel_Arguments --
   ---------------------------

   procedure Scan_Kernel_Arguments
     (Process : not null access
        procedure (Argument : String))
   is
   begin
      if not Have_Kernel_Arguments then
         return;
      end if;

      declare
         Addr         : constant System.Address :=
                          System'To_Address (Header (4));
         Command_Line : String (1 .. 1024);
         pragma Import (Ada, Command_Line);
         for Command_Line'Address use Addr;
         Start : Positive := Command_Line'First;
      begin
         for I in Command_Line'Range loop
            if Command_Line (I) = Character'Val (0)
              or else Command_Line (I) = ' '
            then
               if I > Start then
                  declare
                     Argument : constant String :=
                                  Command_Line (Start .. I - 1);
                  begin
                     Process (Argument);
                  end;
               end if;
               Start := I + 1;
               exit when Command_Line (I) = Character'Val (0);
            end if;
         end loop;
      end;

   end Scan_Kernel_Arguments;

   ---------------------
   -- Scan_Memory_Map --
   ---------------------

   procedure Scan_Memory_Map
     (Process : not null access
        procedure (Available : Boolean;
                   Low       : Rose.Words.Word_64;
                   High      : Rose.Words.Word_64))
   is
      Length  : constant Word_32 := Header (11);
      Current : Word_32 := Header (12);
   begin
      if not Have_Memory_Map then
         return;
      end if;

      if Report_Memory_Map then
         Rose.Boot.Console.Put ("Memory map length: ");
         Rose.Boot.Console.Put (Length);
         Rose.Boot.Console.New_Line;

         Rose.Boot.Console.Put ("Memory map address: ");
         Rose.Boot.Console.Put (Current);
         Rose.Boot.Console.New_Line;
      end if;

      while Current < Header (12) + Header (11) loop
         declare
            Addr : constant System.Address := System'To_Address (Current);
            Map_Entry : Memory_Map_Entry;
            for Map_Entry'Address use Addr;
            pragma Import (Ada, Map_Entry);
            Low : constant Word_64 :=
              Word_64 (Map_Entry.Base_Low)
              + Word_64 (Map_Entry.Base_High) * 2 ** 32;
            High : constant Word_64 :=
              Low
              + Word_64 (Map_Entry.Length_Low)
              + Word_64 (Map_Entry.Length_High) * 2 ** 32;
         begin

            if Report_Memory_Map then
               Rose.Boot.Console.Put ("entry at ");
               Rose.Boot.Console.Put (Current);
               Rose.Boot.Console.Put (": size ");
               Rose.Boot.Console.Put (Map_Entry.Size);
               Rose.Boot.Console.New_Line;
            end if;

            Process
              (Available => Map_Entry.Sort = Memory_Available,
               Low       => Low,
               High      => High);

            Current := Current + Map_Entry.Size + 4;
         end;
      end loop;

   end Scan_Memory_Map;

   ------------------
   -- Scan_Modules --
   ------------------

   procedure Scan_Modules
     (Process : not null access
        procedure (Mod_Start  : Rose.Words.Word_32;
                   Mod_End    : Rose.Words.Word_32;
                   Mod_Text   : String))
   is
      Count   : constant Word_32 := Header (5);
      Mods    : Modules_Array (1 .. Natural (Count));
      for Mods'Address use System'To_Address (Header (6));
      pragma Import (Ada, Mods);
   begin
      if not Have_Memory_Map then
         return;
      end if;

      if Report_Modules then
         Rose.Boot.Console.Put ("Module count: ");
         Rose.Boot.Console.Put (Count);
         Rose.Boot.Console.New_Line;
      end if;

      for Mod_Info of Mods loop
         declare
            Text_Address : constant System.Address :=
              System'To_Address (Mod_Info.Data);
            Text_Buffer : String (1 .. 40);
            for Text_Buffer'Address use Text_Address;
            pragma Import (Ada, Text_Buffer);
            Last : Natural := 0;
         begin
            while Last < Text_Buffer'Last
              and then Text_Buffer (Last + 1) /= Character'Val (0)
            loop
               Last := Last + 1;
            end loop;
            Process (Mod_Info.First, Mod_Info.Last,
                     Text_Buffer (1 .. Last));
         end;
      end loop;

   end Scan_Modules;

   -----------------------------
   -- Word_32_To_Entry_Access --
   -----------------------------

   --  function Word_32_To_Entry_Access
   --    (Addr : Rose.Words.Word_32)
   --     return Memory_Map_Entry_Access
   --  is
   --     function To_Address is new Ada.Unchecked_Conversion
   --       (Source => Rose.Words.Word_32,
   --        Target => System.Address);

   --     function To_Entry_Access is new Ada.Unchecked_Conversion
   --       (Source => Convert.Object_Pointer,
   --        Target => Memory_Map_Entry_Access);

   --  begin
   --     return To_Entry_Access (Convert.To_Pointer
   --                             (To_Address (Addr)));
   --  end Word_32_To_Entry_Access;

end Rose.Multiboot;
