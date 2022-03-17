with System;                            use System;
with System.Machine_Code;

with Rose.Addresses;
with Rose.Words;

with Rose.Boot.Console;
with Rose.Kernel.Heap;

package body Rose.Kernel.Page_Table is

   Log_Page_Table : constant Boolean := False;

   Entries_Per_Page : constant := Physical_Page_Bytes / 4;

   type Page_Entry is
      record
         PFA                  : Physical_Page_Address := 0;
         Available_11         : Boolean               := False;
         Available_10         : Boolean               := False;
         Checkpoint_Read_Only : Boolean               := False;
         Global               : Boolean               := False;
         Zero                 : Boolean               := False;
         Dirty                : Boolean               := False;
         Accessed             : Boolean               := False;
         Cache_Disable        : Boolean               := False;
         Transparent_Write    : Boolean               := False;
         User                 : Boolean               := False;
         Writable             : Boolean               := False;
         Present              : Boolean               := False;
      end record;

   for Page_Entry use
      record
         Present              at 0 range 0 .. 0;
         Writable             at 0 range 1 .. 1;
         User                 at 0 range 2 .. 2;
         Transparent_Write    at 0 range 3 .. 3;
         Cache_Disable        at 0 range 4 .. 4;
         Accessed             at 0 range 5 .. 5;
         Dirty                at 0 range 6 .. 6;
         Zero                 at 0 range 7 .. 7;
         Global               at 1 range 0 .. 0;
         Checkpoint_Read_Only at 1 range 1 .. 1;
         Available_10         at 1 range 2 .. 2;
         Available_11         at 1 range 3 .. 3;
         PFA                  at 1 range 4 .. 23;
      end record;

   for Page_Entry'Size use 32;

   type Page_Entry_Index is range 0 .. Entries_Per_Page - 1;

   First_Kernel_Page_Entry_Index : constant :=
                                     (Entries_Per_Page / 4) * 3;

   subtype Page_Directory_Index is Page_Entry_Index;

   subtype User_Page_Directory_Index is
     Page_Directory_Index range
       0 .. First_Kernel_Page_Entry_Index - 1;

   subtype Kernel_Page_Directory_Index is
     Page_Directory_Index range
       First_Kernel_Page_Entry_Index .. Entries_Per_Page - 1;

   type Page_Table_Array is
     array (Page_Entry_Index) of Page_Entry
     with Pack, Size => Entries_Per_Page * 32;

   Page_Directory    : Page_Table_Array;
   pragma Import (C, Page_Directory, "boot_page_directory");

   --  Kernel_Page_Table : Page_Table_Array;
   --  pragma Import (C, Kernel_Page_Table, "KernPageTbl");

--     type Page_Directory_Allocation is array (0 .. 1023) of Boolean;
--     pragma Pack (Page_Directory_Allocation);

   --  how many pages we allocate in a block
   Page_Table_Block_Size : constant := 64;

   procedure Init_Page_Entry (P : in out Page_Entry);

   function Page_Table_Block_Start (Dir_Offset : Natural)
                                    return Natural
     with Unreferenced;

--     procedure Allocate_Page_Table_Block (Start : Natural);

   procedure Show_Page_Entry (Page : Page_Entry);

   procedure Map_Page
     (Directory      : in out Page_Table_Array;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address;
      Physical_Page  : Rose.Addresses.Physical_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      User           : Boolean);

   procedure Unmap_Page
     (Directory      : in out Page_Table_Array;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address);

   -----------------------------
   -- Activate_Page_Directory --
   -----------------------------

   procedure Activate_Page_Directory
     (Directory_Page : Rose.Addresses.Virtual_Page_Address)
   is
      use System.Machine_Code;
      use Rose.Words;
      Directory_Address : constant Rose.Addresses.Virtual_Address :=
                            Rose.Addresses.Virtual_Page_To_Address
                              (Directory_Page);
      Physical_Address  : constant Rose.Addresses.Physical_Address :=
                            Rose.Kernel.Heap.Get_Physical_Address
                              (Directory_Address);
   begin
      Asm ("mov %0, %%eax",
           Inputs   => Word_32'Asm_Input ("g", Word_32 (Physical_Address)),
           Volatile => True);
      Asm ("mov %%eax, %%cr3", Volatile => True);
   end Activate_Page_Directory;

   -------------------------
   -- Allocate_Page_Block --
   -------------------------

--     procedure Allocate_Page_Table_Block (Start : Natural) is
--        Table_Addr : constant Virtual_Address :=
--          Rose.Kernel.Heap.Allocate (Page_Table_Block_Size *
--                                       Physical_Page_Bytes,
--                                     Physical_Page_Bytes);
--        Dir_Entry : Page_Entry;
--     begin
--        Init_Page_Entry (Dir_Entry);
--        Dir_Entry.PFA     := Page_Frame_Address (Table_Addr / 4096);
--        for I in 0 .. Page_Table_Block_Size loop
--           if not Allocated (Start + I) then
--              Allocated (Start + I) := True;
--              Page_Directory (Start + I) := Dir_Entry;
--           end if;
--           Dir_Entry.PFA := Dir_Entry.PFA + 1;
--        end loop;
--     end Allocate_Page_Table_Block;

   -------------------------
   -- Disable_Page_Writes --
   -------------------------

   procedure Disable_Page_Writes
     (Directory_Page : Rose.Addresses.Virtual_Page_Address)
   is
      Directory : Page_Table_Array;
      pragma Import (Ada, Directory);
      for Directory'Address use
        System'To_Address (Virtual_Page_To_Address (Directory_Page));
   begin
      for Directory_Entry of
        Directory (0 .. First_Kernel_Page_Entry_Index - 1)
      loop
         if Directory_Entry.Present then
            declare
               Phys_Addr  : constant Physical_Address :=
                              Physical_Address
                                (Directory_Entry.PFA) * Physical_Page_Bytes;
               Addr       : constant Virtual_Address :=
                              Rose.Kernel.Heap.Get_Virtual_Address (Phys_Addr);
               Table_Page : Page_Table_Array;
               for Table_Page'Address use System'To_Address (Addr);
               pragma Import (Ada, Table_Page);
            begin
               for Table_Entry of Table_Page loop
                  if Table_Entry.Present
                    and then Table_Entry.Writable
                  then
                     Table_Entry.Writable := False;
                     Table_Entry.Checkpoint_Read_Only := True;
                     if Log_Checkpoint_Pages then
                        Show_Page_Entry (Table_Entry);
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end loop;

   end Disable_Page_Writes;

   ------------------------
   -- Enable_Page_Writes --
   ------------------------

   procedure Enable_Page_Writes
     (Directory_Page : Rose.Addresses.Virtual_Page_Address)
   is
      Directory : Page_Table_Array;
      pragma Import (Ada, Directory);
      for Directory'Address use
        System'To_Address (Virtual_Page_To_Address (Directory_Page));
   begin
      for Directory_Entry of
        Directory (0 .. First_Kernel_Page_Entry_Index - 1)
      loop
         if Directory_Entry.Present then
            declare
               Phys_Addr  : constant Physical_Address :=
                              Physical_Address
                                (Directory_Entry.PFA) * Physical_Page_Bytes;
               Addr       : constant Virtual_Address :=
                              Rose.Kernel.Heap.Get_Virtual_Address (Phys_Addr);
               Table_Page : Page_Table_Array;
               for Table_Page'Address use System'To_Address (Addr);
               pragma Import (Ada, Table_Page);
            begin
               for Table_Entry of Table_Page loop
                  if Table_Entry.Present
                    and then Table_Entry.Checkpoint_Read_Only
                  then
                     Table_Entry.Writable := True;
                     Table_Entry.Checkpoint_Read_Only := False;
                     if Log_Checkpoint_Pages then
                        Show_Page_Entry (Table_Entry);
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end loop;
   end Enable_Page_Writes;

   ---------------------
   -- Init_Page_Entry --
   ---------------------

   procedure Init_Page_Entry (P : in out Page_Entry) is
   begin
      P.PFA               := 0;
      P.Available_11      := False;
      P.Available_10      := False;
      P.Available_9       := False;
      P.Global            := False;
      P.Zero              := False;
      P.Dirty             := False;
      P.Accessed          := False;
      P.Cache_Disable     := False;
      P.Transparent_Write := False;
      P.User              := True;
      P.Writable          := False;
      P.Present           := True;
   end Init_Page_Entry;

   ---------------------
   -- Init_Page_Table --
   ---------------------

   procedure Init_Page_Table is
   begin
      Page_Directory (User_Page_Directory_Index) :=
        (others => <>);
   end Init_Page_Table;

   ------------------------------
   -- Init_User_Page_Directory --
   ------------------------------

   procedure Init_User_Page_Directory
     (Directory_Page : Rose.Addresses.Virtual_Page_Address)
   is
      Directory : Page_Table_Array;
      pragma Import (Ada, Directory);
      for Directory'Address use
        System'To_Address (Virtual_Page_To_Address (Directory_Page));
   begin
      Directory := (others => <>);
      --  top 1G always mapped to Kernel space
      Directory (Kernel_Page_Directory_Index) :=
        Page_Directory (Kernel_Page_Directory_Index);
   end Init_User_Page_Directory;

   ---------------------------
   -- Kernel_Page_Directory --
   ---------------------------

   function Kernel_Page_Directory
     return Rose.Addresses.Physical_Address
   is (Rose.Addresses.Physical_Address
         (Rose.Addresses.To_Virtual_Address (Page_Directory'Address)
          - Kernel_Virtual_Base));

   ---------------------
   -- Map_Kernel_Page --
   ---------------------

   procedure Map_Kernel_Page
     (Virtual_Page   : Rose.Addresses.Virtual_Page_Address;
      Physical_Page  : Rose.Addresses.Physical_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      User           : Boolean)
   is
   begin
      Map_Page (Page_Directory, Virtual_Page, Physical_Page,
                Readable, Writable, Executable, User);
   end Map_Kernel_Page;

   --------------
   -- Map_Page --
   --------------

   procedure Map_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address;
      Physical_Page  : Rose.Addresses.Physical_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      User           : Boolean)
   is
      Directory : Page_Table_Array;
      pragma Import (Ada, Directory);
      for Directory'Address use
        System'To_Address (Virtual_Page_To_Address (Directory_Page));
   begin
      Map_Page (Directory, Virtual_Page, Physical_Page,
                Readable, Writable, Executable, User);
   end Map_Page;

   --------------
   -- Map_Page --
   --------------

   procedure Map_Page
     (Directory      : in out Page_Table_Array;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address;
      Physical_Page  : Rose.Addresses.Physical_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      User           : Boolean)
   is
      pragma Unreferenced (Executable, Readable);   --   :-(
      Directory_Index : constant Page_Entry_Index :=
                          Page_Entry_Index (Virtual_Page / 1024);
      Table_Index : constant Page_Entry_Index :=
                      Page_Entry_Index (Virtual_Page mod 1024);
   begin
      if Log_Page_Table then
         Rose.Boot.Console.Put ("Map_Page: ");
         Rose.Boot.Console.Put
           (Physical_Address (Virtual_Page) * 4096);
         Rose.Boot.Console.Put (" --> ");
         Rose.Boot.Console.Put (Physical_Address (Physical_Page) * 4096);
         Rose.Boot.Console.New_Line;
      end if;

      if Directory (Directory_Index).PFA = 0 then
         declare
            Addr       : constant Virtual_Page_Address :=
                           Rose.Kernel.Heap.Allocate_Page;
            Table_Page : Page_Table_Array;
            pragma Import (Ada, Table_Page);
            for Table_Page'Address use
              System'To_Address (Virtual_Page_To_Address (Addr));
         begin
            if Log_Page_Table then
               Rose.Boot.Console.Put ("initialising table page");
            end if;

            Table_Page := (others => <>);
            Init_Page_Entry (Directory (Directory_Index));
            Directory (Directory_Index).PFA :=
              Rose.Kernel.Heap.Get_Physical_Page (Addr);
            Directory (Directory_Index).User := User;
            Directory (Directory_Index).Writable := True;
         end;
      end if;

      if False and then Log_Page_Table then
         Show_Page_Entry (Directory (Directory_Index));
      end if;

      declare
         Phys_Addr : constant Physical_Address :=
                       Physical_Address (Directory (Directory_Index).PFA)
                       * Physical_Page_Bytes;
         Addr      : constant Virtual_Address :=
                       Rose.Kernel.Heap.Get_Virtual_Address (Phys_Addr);
         Table_Page : Page_Table_Array;
         for Table_Page'Address use System'To_Address (Addr);
         pragma Import (Ada, Table_Page);
         Table_Entry : Page_Entry renames Table_Page (Table_Index);
      begin
         Init_Page_Entry (Table_Entry);
         Table_Entry.PFA := Physical_Page;
         Table_Entry.Writable := Writable;
         Table_Entry.User := User;
         if Log_Page_Table then
            Show_Page_Entry (Table_Entry);
         end if;
      end;

   end Map_Page;

   --------------------------
   -- Mapped_Physical_Page --
   --------------------------

   function Mapped_Physical_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
      return Rose.Addresses.Physical_Page_Address
   is
      Directory : Page_Table_Array;
      pragma Import (Ada, Directory);
      for Directory'Address use
        System'To_Address (Virtual_Page_To_Address (Directory_Page));

      Directory_Index : constant Page_Entry_Index :=
                          Page_Entry_Index (Virtual_Page / 1024);
      Table_Index     : constant Page_Entry_Index :=
                          Page_Entry_Index (Virtual_Page mod 1024);
   begin

      if Directory (Directory_Index).PFA = 0 then
         return 0;
      end if;

      declare
         Phys_Addr   : constant Physical_Address :=
                         Physical_Address (Directory (Directory_Index).PFA)
                         * Physical_Page_Bytes;
         Addr        : constant Virtual_Address :=
                         Rose.Kernel.Heap.Get_Virtual_Address (Phys_Addr);
         Table_Page  : Page_Table_Array;
         for Table_Page'Address use System'To_Address (Addr);
         pragma Import (Ada, Table_Page);
         Table_Entry : Page_Entry renames Table_Page (Table_Index);
      begin
         return Table_Entry.PFA;
      end;
   end Mapped_Physical_Page;

   ----------------------------
   -- Page_Table_Block_Start --
   ----------------------------

   function Page_Table_Block_Start (Dir_Offset : Natural)
                                   return Natural
   is
   begin
      return Dir_Offset - Dir_Offset mod Page_Table_Block_Size;
   end Page_Table_Block_Start;

   -------------------------
   -- Report_Mapped_Pages --
   -------------------------

   procedure Report_Mapped_Pages
     (Directory_Page : Rose.Addresses.Physical_Address)
   is
      use Rose.Words;
      Directory_Address : constant Virtual_Address :=
                            Rose.Kernel.Heap.Get_Virtual_Address
                              (Directory_Page);
      Directory         : Page_Table_Array;
      pragma Import (Ada, Directory);
      for Directory'Address use System'To_Address (Directory_Address);
   begin
      Rose.Boot.Console.Put ("directory page: ");
      Rose.Boot.Console.Put (Rose.Words.Word (Directory_Page));
      Rose.Boot.Console.New_Line;

      for Index in Directory'Range loop

         if Directory (Index).Present then

            declare
               Base : constant Word_32 :=
                        Word_32 (Index) * 4096 * 1024;
            begin
               Rose.Boot.Console.Put (Base);
               Rose.Boot.Console.Put ("  ");
               Show_Page_Entry (Directory (Index));

               if Index not in 16#0300# .. 16#03E0# then
                  declare
                     Page_Entry_Address : constant Virtual_Address :=
                                          Rose.Kernel.Heap.Get_Virtual_Address
                                              (Physical_Address
                                                 (Directory (Index).PFA)
                                               * 4096);
                     Page_Table         : Page_Table_Array;
                     pragma Import (Ada, Page_Table);
                     for Page_Table'Address use
                       System'To_Address (Page_Entry_Address);
                  begin
                     for Index in Page_Table'Range loop
                        if Page_Table (Index).Present then
                           Rose.Boot.Console.Put ("   ");
                           Rose.Boot.Console.Put
                             (Base + Word_32 (Index) * 4096);
                           Rose.Boot.Console.Put ("  ");
                           Show_Page_Entry (Page_Table (Index));
                        end if;
                     end loop;
                  end;
               end if;
            end;
         end if;
      end loop;
   end Report_Mapped_Pages;

   ---------------------
   -- Show_Page_Entry --
   ---------------------

   procedure Show_Page_Entry (Page : Page_Entry) is
      use Rose.Words;
      W  : Word_32;
      for W'Address use Page'Address;
      pragma Import (Ada, W);

      procedure Put_Flag (Value : Boolean;
                          Name  : String);

      --------------
      -- Put_Flag --
      --------------

      procedure Put_Flag (Value : Boolean;
                          Name  : String)
      is
      begin
         Rose.Boot.Console.Put (if Value then Name else "-");
      end Put_Flag;

   begin
      Rose.Boot.Console.Put (W);
      Rose.Boot.Console.Put (": ");
      Rose.Boot.Console.Put (Word_32 (Page.PFA) * 4096);
      Rose.Boot.Console.Put (" ");
      Put_Flag (Page.Available_11, "z");
      Put_Flag (Page.Available_10, "y");
      Put_Flag (Page.Checkpoint_Read_Only, "r");
      Put_Flag (Page.Global, "g");
      Put_Flag (Page.Zero, "0");
      Put_Flag (Page.Dirty, "d");
      Put_Flag (Page.Accessed, "a");
      Put_Flag (Page.Cache_Disable, "c");
      Put_Flag (Page.Transparent_Write, "t");
      Put_Flag (Page.User, "u");
      Put_Flag (Page.Writable, "w");
      Put_Flag (Page.Present, "p");
      Rose.Boot.Console.New_Line;
   end Show_Page_Entry;

   -----------------------
   -- Unmap_Kernel_Page --
   -----------------------

   procedure Unmap_Kernel_Page
     (Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
   is
   begin
      Unmap_Page (Page_Directory, Virtual_Page);
   end Unmap_Kernel_Page;

   ----------------
   -- Unmap_Page --
   ----------------

   procedure Unmap_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
   is
      Directory : Page_Table_Array;
      pragma Import (Ada, Directory);
      for Directory'Address use
        System'To_Address (Virtual_Page_To_Address (Directory_Page));

   begin
      Unmap_Page (Directory, Virtual_Page);
   end Unmap_Page;

   ----------------
   -- Unmap_Page --
   ----------------

   procedure Unmap_Page
     (Directory      : in out Page_Table_Array;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
   is
      Directory_Index : constant Page_Entry_Index :=
                          Page_Entry_Index (Virtual_Page / 1024);
      Table_Index     : constant Page_Entry_Index :=
                          Page_Entry_Index (Virtual_Page mod 1024);
      Phys_Addr       : constant Physical_Address :=
                          Physical_Address (Directory (Directory_Index).PFA)
                          * Physical_Page_Bytes;
      Addr            : constant Virtual_Address :=
                          Rose.Kernel.Heap.Get_Virtual_Address (Phys_Addr);
      Table_Page      : Page_Table_Array;
      for Table_Page'Address use System'To_Address (Addr);
      pragma Import (Ada, Table_Page);
      Table_Entry     : Page_Entry renames Table_Page (Table_Index);
   begin
      Table_Entry := (others => <>);
   end Unmap_Page;

end Rose.Kernel.Page_Table;
