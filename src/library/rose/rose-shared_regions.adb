with System.Storage_Elements;

with Rose.Capabilities.Standard;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Segment;

package body Rose.Shared_Regions is

   use Rose.Words;
   use type System.Storage_Elements.Integer_Address;

   Page_Size : constant := 4096;

   Window_Base  : constant Word := 16#C_0000#;
   Window_Bound : constant Word := 16#D_0000#;
   --  The pages kept for shared segments.

   Max_Mappings : constant := 32;
   --  A process has at most 32 segments, so it can't have more mappings.

   type Mapping is
      record
         Base  : Word := 0;
         Bound : Word := 0;
      end record;
   --  The pages Base .. Bound - 1 are mapped. A free entry has Bound 0.

   Mappings : array (1 .. Max_Mappings) of Mapping;

   Self   : Rose.Interfaces.Process.Client.Process_Client;
   Opened : Boolean := False;
   --  This process's own Process interface, opened when it is first
   --  needed.

   function Find_Space (Pages : Word) return Word;
   --  The first page of Pages free pages in the window, or zero if there
   --  aren't that many together.

   procedure Open;
   --  Open Self, if it hasn't been.

   function Page_Of (Address : System.Address) return Word
   is (Word (System.Storage_Elements.To_Integer (Address) / Page_Size));

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

   ---------
   -- Map --
   ---------

   function Map
     (Region   : in out Rose.Interfaces.Region.Region_Object'Class;
      Writable : Boolean := True)
      return System.Address
   is
      use Rose.Interfaces.Segment;
      use type Rose.Objects.Object_Id;
      Base_Page, Bound_Page : Rose.Objects.Object_Id;
   begin
      Region.Get_Range (Base_Page, Bound_Page);

      if Bound_Page <= Base_Page
        or else Bound_Page - Base_Page
          > Rose.Objects.Object_Id (Window_Bound - Window_Base)
      then
         raise Mapping_Error with "the region is too large to map";
      end if;

      declare
         Pages : constant Word := Word (Bound_Page - Base_Page);
         Base  : constant Word := Find_Space (Pages);
      begin
         if Base = 0 then
            raise Mapping_Error with "no room to map the region";
         end if;

         for Item of Mappings loop
            if Item.Bound = 0 then
               Open;
               Self.Add_Segment
                 (Virtual_Base  => Base,
                  Virtual_Bound => Base + Pages,
                  Region        => Region,
                  Region_Offset => 0,
                  Flags         =>
                    Segment_Readable + Segment_Shared
                  + (if Writable then Segment_Writable else 0));
               Item := (Base, Base + Pages);
               return System.Storage_Elements.To_Address
                 (System.Storage_Elements.Integer_Address (Base) * Page_Size);
            end if;
         end loop;

         raise Mapping_Error with "too many shared mappings";
      end;
   end Map;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      if not Opened then
         Rose.Interfaces.Process.Client.Open
           (Self, Rose.Capabilities.Standard.Process_Interface_Cap);
         Opened := True;
      end if;
   end Open;

   -----------
   -- Unmap --
   -----------

   procedure Unmap (Address : System.Address) is
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

      raise Mapping_Error with "no shared mapping at that address";
   end Unmap;

end Rose.Shared_Regions;
