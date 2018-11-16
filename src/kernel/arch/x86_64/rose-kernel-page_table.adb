with System;                            use System;

with Rose.Addresses;
with Rose.Words;

with Rose.Boot.Console;
with Rose.Kernel.Heap;
with Rose.Kernel.Panic;

package body Rose.Kernel.Page_Table is

   Entries_Per_Page : constant := Physical_Page_Bytes / 4;

   type Page_Frame_Address is mod 2 ** 20;
   for Page_Frame_Address'Size use 20;

   type Page_Entry is
      record
         PFA               : Page_Frame_Address;
         Available_11      : Boolean;
         Available_10      : Boolean;
         Available_9       : Boolean;
         Zero              : Boolean;
         L                 : Boolean;
         Dirty             : Boolean;
         Accessed          : Boolean;
         Cache_Disable     : Boolean;
         Transparent_Write : Boolean;
         User              : Boolean;
         Writable          : Boolean;
         Present           : Boolean;
      end record;

   for Page_Entry use
      record
         Present           at 0 range 0 .. 0;
         Writable          at 0 range 1 .. 1;
         User              at 0 range 2 .. 2;
         Transparent_Write at 0 range 3 .. 3;
         Cache_Disable     at 0 range 4 .. 4;
         Accessed          at 0 range 5 .. 5;
         Dirty             at 0 range 6 .. 6;
         L                 at 0 range 7 .. 7;
         Zero              at 1 range 0 .. 0;
         Available_9       at 1 range 1 .. 1;
         Available_10      at 1 range 2 .. 2;
         Available_11      at 1 range 3 .. 3;
         PFA               at 1 range 4 .. 23;
      end record;

   for Page_Entry'Size use 32;

   type Page_Table_Array is
     array (0 .. Entries_Per_Page - 1) of Page_Entry
     with Pack, Size => 4096 * 8;


   Page_Directory    : Page_Table_Array;
   pragma Import (C, Page_Directory, "boot_PML4T");

   --  Kernel_Page_Table : Page_Table_Array;
   --  pragma Import (C, Kernel_Page_Table, "KernPageTbl");

   type Page_Directory_Allocation is array (0 .. 1023) of Boolean;
   pragma Pack (Page_Directory_Allocation);
   Allocated : Page_Directory_Allocation;

   --  how many pages we allocate in a block
   Page_Table_Block_Size : constant := 64;

   procedure Init_Page_Entry (P : in out Page_Entry);

   function Page_Table_Block_Start (Dir_Offset : Natural)
                                   return Natural;

   procedure Allocate_Page_Table_Block (Start : Natural);

   procedure Show_Page_Entry (Page : Page_Entry);

   -------------------------
   -- Allocate_Page_Block --
   -------------------------

   procedure Allocate_Page_Table_Block (Start : Natural) is
      Table_Addr : constant Virtual_Address :=
        Rose.Kernel.Heap.Allocate (Page_Table_Block_Size *
                                     Physical_Page_Bytes,
                                   Physical_Page_Bytes);
      Dir_Entry : Page_Entry;
   begin
      Init_Page_Entry (Dir_Entry);
      Dir_Entry.PFA     := Page_Frame_Address (Table_Addr / 4096);
      for I in 0 .. Page_Table_Block_Size loop
         if not Allocated (Start + I) then
            Allocated (Start + I) := True;
            Page_Directory (Start + I) := Dir_Entry;
         end if;
         Dir_Entry.PFA := Dir_Entry.PFA + 1;
      end loop;
   end Allocate_Page_Table_Block;


   ---------------------
   -- Init_Page_Entry --
   ---------------------

   procedure Init_Page_Entry (P : in out Page_Entry) is
   begin
      P.PFA               := 0;
      P.Available_11      := False;
      P.Available_10      := False;
      P.Available_9       := False;
      P.Zero              := False;
      P.L                 := False;
      P.Dirty             := False;
      P.Accessed          := False;
      P.Cache_Disable     := False;
      P.Transparent_Write := False;
      P.User              := True;
      P.Writable          := False;
      P.Present           := True;
   end Init_Page_Entry;

   ----------------------
   -- Init_Page_Tables --
   ----------------------

   procedure Init_Page_Table is
   begin
      --  note that page directory zero was allocated, but later dropped
      Allocated := (0       => True,   --  allocated by the boot process
                    16#300# => True,   --  allocated by the boot process
                    others  => False);

      Show_Page_Entry (Page_Directory (0));
   end Init_Page_Table;

   --------------
   -- Map_Page --
   --------------

   procedure Map_Page (Physical_Page : Physical_Page_Address;
                       Virtual_Page  : Virtual_Page_Address;
                       Present       : Boolean;
                       Writable      : Boolean;
                       Executable    : Boolean;
                       User          : Boolean)
   is
      pragma Unreferenced (Executable);   --   :-(
      Directory_Offset : constant Natural := Natural (Virtual_Page / 1024);
      Table_Offset     : constant Natural := Natural (Virtual_Page mod 1024);
      Directory_Entry  : Page_Entry := Page_Directory (Directory_Offset);
      New_Entry : Page_Entry;
   begin
      Rose.Boot.Console.Put ("Map_Page: ");
      Rose.Boot.Console.Put
        (Physical_Address (Virtual_Page));
      Rose.Boot.Console.Put (" --> ");
      Rose.Boot.Console.Put (Physical_Address (Physical_Page));
      Rose.Boot.Console.New_Line;

      Init_Page_Entry (New_Entry);
      New_Entry.PFA      := Page_Frame_Address (Physical_Page);
      New_Entry.Present  := Present;
      New_Entry.Writable := Writable;
      New_Entry.User     := User;

      if not Allocated (Directory_Offset) then
         --  no tables allocated for this region yet
         --  create a new region around it
         Rose.Boot.Console.Put ("Allocating new region for ");
         Rose.Boot.Console.Put (Rose.Words.Word_32 (Directory_Offset));
         Allocate_Page_Table_Block (Page_Table_Block_Start (Directory_Offset));
         Directory_Entry := Page_Directory (Directory_Offset);
         if not Directory_Entry.Present then
            Rose.Kernel.Panic.Panic ("Cannot allocate physical page",
                                     Physical_Page_To_Address (Physical_Page));
         end if;

      end if;

      declare
         Table_Address : constant System.Address :=
           System'To_Address (Directory_Entry.PFA * 4);
         Table         : Page_Table_Array;
         for Table'Address use Table_Address;
         pragma Import (Ada, Table);
      begin
         Show_Page_Entry (New_Entry);
         Table (Table_Offset) := New_Entry;
      end;

   end Map_Page;

   ----------------------------
   -- Page_Table_Block_Start --
   ----------------------------

   function Page_Table_Block_Start (Dir_Offset : Natural)
                                   return Natural
   is
   begin
      return Dir_Offset - Dir_Offset mod Page_Table_Block_Size;
   end Page_Table_Block_Start;

   ---------------------
   -- Show_Page_Entry --
   ---------------------

   procedure Show_Page_Entry (Page : Page_Entry) is
      use Rose.Words;
      Bc : constant array (Boolean) of Character := ('0', '1');
      W  : Word_32;
      for W'Address use Page'Address;
      pragma Import (Ada, W);
   begin
      Rose.Boot.Console.Put ("Page Entry: ");
      Rose.Boot.Console.Put (W);
      Rose.Boot.Console.Put (" -- ");
      Rose.Boot.Console.Put (Word_32 (Page.PFA) * 4096);
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Available_11));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Available_10));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Available_9));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Zero));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.L));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Dirty));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Accessed));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Cache_Disable));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Transparent_Write));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.User));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Writable));
      Rose.Boot.Console.Put (":");
      Rose.Boot.Console.Put (Bc (Page.Present));
      Rose.Boot.Console.New_Line;
   end Show_Page_Entry;

end Rose.Kernel.Page_Table;
