with Rose.Capabilities.Standard;
with Rose.Words;

with Rose.Interfaces.Processes.Process.Client;
with Rose.Interfaces.Processes.Segment;

package body Rose.Unsaved_Memory is

   use Rose.Words;
   use System.Storage_Elements;

   Page_Size : constant := 4096;

   Window_Base  : constant Word := 16#D_0000#;
   Window_Bound : constant Word := 16#D_1000#;
   --  The pages kept for unsaved segments.

   Max_Mappings : constant := 8;

   type Mapping is
      record
         Base  : Word := 0;
         Bound : Word := 0;
      end record;
   --  The pages Base .. Bound - 1 are mapped. A free entry has Bound 0.

   Mappings : array (1 .. Max_Mappings) of Mapping;

   Self   : Rose.Interfaces.Processes.Process.Client.Process_Client;
   Opened : Boolean := False;
   --  This process's own Process interface, opened when it is first
   --  needed.

   function Find_Space (Pages : Word) return Word;
   --  The first page of Pages free pages in the window, or zero if there
   --  aren't that many together.

   procedure Open;
   --  Open Self, if it hasn't been.

   function Page_Of (Address : System.Address) return Word
   is (Word (To_Integer (Address) / Page_Size));

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Size : System.Storage_Elements.Storage_Count)
      return System.Address
   is
      use Rose.Interfaces.Processes.Segment;
   begin
      if Size = 0
        or else Size > Storage_Count (Window_Bound - Window_Base) * Page_Size
      then
         raise Mapping_Error with "no unsaved segment can be that size";
      end if;

      declare
         Pages : constant Word :=
                   Word ((Size + Page_Size - 1) / Page_Size);
         Base  : constant Word := Find_Space (Pages);
      begin
         if Base = 0 then
            raise Mapping_Error with "no room for the unsaved segment";
         end if;

         for Item of Mappings loop
            if Item.Bound = 0 then
               Open;
               Self.Add_Nonpersistent_Segment
                 (Virtual_Base  => Base,
                  Virtual_Bound => Base + Pages,
                  Flags         => Segment_Readable + Segment_Writable);
               Item := (Base, Base + Pages);
               return To_Address (Integer_Address (Base) * Page_Size);
            end if;
         end loop;

         raise Mapping_Error with "too many unsaved segments";
      end;
   end Allocate;

   ----------------
   -- Find_Space --
   ----------------

   function Find_Space (Pages : Word) return Word is
      Candidate : Word := Window_Base;
      Moved     : Boolean := True;
   begin
      --  Move past each mapping that overlaps, until none does.

      while Moved loop
         if Pages > Window_Bound - Candidate then
            return 0;
         end if;

         Moved := False;
         for Item of Mappings loop
            if Item.Bound /= 0
              and then Item.Base < Candidate + Pages
              and then Candidate < Item.Bound
            then
               Candidate := Item.Bound;
               Moved := True;
            end if;
         end loop;
      end loop;

      return Candidate;
   end Find_Space;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      if not Opened then
         Rose.Interfaces.Processes.Process.Client.Open
           (Self, Rose.Capabilities.Standard.Process_Interface_Cap);
         Opened := True;
      end if;
   end Open;

   -------------
   -- Release --
   -------------

   procedure Release (Address : System.Address) is
      Base : constant Word := Page_Of (Address);
   begin
      for Item of Mappings loop
         if Item.Bound /= 0 and then Item.Base = Base then
            Open;
            Self.Remove_Segment (Base);
            Item := (others => <>);
            return;
         end if;
      end loop;

      raise Mapping_Error with "no unsaved segment at that address";
   end Release;

end Rose.Unsaved_Memory;
