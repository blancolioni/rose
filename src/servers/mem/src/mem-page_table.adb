with Rose.Console_IO;
with Rose.Objects;
--  with Rose.Words;

with Mem.Calls;
with Mem.Processes;

package body Mem.Page_Table is

   --  Max_Mapped_Pages : constant := 65536;

   type Mapped_Page_Record is
      record
         Process    : Process_Id;
         Virt       : Rose.Addresses.Virtual_Page_Address;
         Phys       : Rose.Addresses.Physical_Page_Address;
         Dirty      : Boolean;
         Mapped     : Boolean;
         Readable   : Boolean;
         Writable   : Boolean;
         Executable : Boolean;
      end record
   with Pack, Size => 64;

   type Mapped_Page_Array is array (Cursor range <>) of Mapped_Page_Record
     with Pack;

   Mapped_Pages     : Mapped_Page_Array (1 .. 2000);
   --  Sorted_Pages     : array (1 .. 2000) of Cursor;
   Mapped_Page_Last : Cursor := 0;
   Free_Count       : Natural := 0;

   Dirty_Pages     : array (1 .. 2000) of Cursor;
   Dirty_Page_Last : Natural := 0;

   -----------------------
   -- Clear_Dirty_Pages --
   -----------------------

   procedure Clear_Dirty_Pages is
   begin
      for I in 1 .. Dirty_Page_Last loop
         Mapped_Pages (Dirty_Pages (I)).Dirty := False;
      end loop;
      Dirty_Page_Last := 0;
   end Clear_Dirty_Pages;

   ----------------
   -- Delete_All --
   ----------------

   procedure Delete_All
     (Process : Process_Id)
   is
   begin
      for I in 1 .. Mapped_Page_Last loop
         declare
            Element : Mapped_Page_Record renames Mapped_Pages (I);
         begin
            if Element.Process = Process then
               Element.Process := 0;
               Free_Count := Free_Count + 1;
            end if;
         end;
      end loop;
   end Delete_All;

   ----------
   -- Find --
   ----------

   function Find
     (Process    : Process_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address)
      return Cursor
   is
      use type Rose.Addresses.Virtual_Page_Address;
   begin
      for I in 1 .. Mapped_Page_Last loop
         declare
            Element : Mapped_Page_Record renames Mapped_Pages (I);
         begin
            if Element.Process = Process
              and then Element.Virt = Virtual
            then
               return I;
            end if;
         end;
      end loop;
      return 0;
   end Find;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Process    : Process_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Physical   : Rose.Addresses.Physical_Page_Address;
      Mapped     : Boolean;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
   is
      Position : Cursor := 0;
   begin

      if Free_Count > 0 then
         for I in 1 .. Mapped_Page_Last loop
            if Mapped_Pages (I).Process = 0 then
               Position := I;
               Free_Count := Free_Count - 1;
               exit;
            end if;
         end loop;
      end if;

      if Position = 0 then
         if Mapped_Page_Last = Mapped_Pages'Last then
            Rose.Console_IO.Put_Line ("mem: too many mapped pages");
            return;
         end if;

         Mapped_Page_Last := Mapped_Page_Last + 1;
         Position := Mapped_Page_Last;
      end if;

      Mapped_Pages (Position) :=
        Mapped_Page_Record'
          (Process    => Process,
           Virt       => Virtual,
           Phys       => Physical,
           Mapped     => Mapped,
           Readable   => Readable,
           Writable   => Writable,
           Executable => Executable,
           Dirty      => False);

   end Insert;

   --------------
   -- Is_Dirty --
   --------------

   function Is_Dirty (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Dirty;
   end Is_Dirty;

   -------------------
   -- Is_Executable --
   -------------------

   function Is_Executable (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Executable;
   end Is_Executable;

   ---------------
   -- Is_Mapped --
   ---------------

   function Is_Mapped (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Mapped;
   end Is_Mapped;

   -----------------
   -- Is_Readable --
   -----------------

   function Is_Readable (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Readable;
   end Is_Readable;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Writable;
   end Is_Writable;

   -------------------------
   -- Iterate_Dirty_Pages --
   -------------------------

   procedure Iterate_Dirty_Pages
     (Process : not null access procedure (Position : Cursor))
   is
   begin
      for I in 1 .. Dirty_Page_Last loop
         Process (Dirty_Pages (I));
      end loop;
   end Iterate_Dirty_Pages;

   ----------------------
   -- Physical_Address --
   ----------------------

   function Physical_Address
     (Position : Cursor) return Rose.Addresses.Physical_Page_Address
   is
   begin
      return Mapped_Pages (Position).Phys;
   end Physical_Address;

   --------------------------
   -- Set_Physical_Address --
   --------------------------

   procedure Set_Physical_Address
     (Position : Cursor; Phys : Rose.Addresses.Physical_Page_Address)
   is
   begin
      Mapped_Pages (Position).Phys := Phys;
   end Set_Physical_Address;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (Position : Cursor) is
      Page : Mapped_Page_Record renames Mapped_Pages (Position);
   begin
      Page.Writable := True;
      Dirty_Page_Last := Dirty_Page_Last + 1;
      Dirty_Pages (Dirty_Page_Last) := Position;
      Mem.Calls.Map
        (Process    =>
           Mem.Processes.Get_Object_Id
             (Rose.Objects.Capability_Identifier (Page.Process)),
         Physical   => Page.Phys,
         Virtual    => Page.Virt,
         Readable   => Page.Readable,
         Writeable  => Page.Writable,
         Executable => Page.Executable);
   end Set_Writable;

end Mem.Page_Table;
