with Ada.Unchecked_Deallocation;

with Rose.Interfaces.Constructor.Client;
with Rose.Interfaces.Releasable_Cap_Set.Client;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Releasable_Reader.Server;
with Rose.Interfaces.Replaceable.Client;
with Rose.Interfaces.Replaceable.Server;
with Rose.Invocation;
with Rose.Streams;
with Rose.Words;

package body Rose.Replacement is

   use Ada.Streams;
   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word_32;

   Magic   : constant Rose.Words.Word_32 := 16#5245_504C#;
   Version : constant Rose.Words.Word_32 := 1;
   --  The header of a state, and the version of this package's part.

   type Element_Array_Access is access Stream_Element_Array;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Stream_Element_Array, Element_Array_Access);

   --  A stream into memory, which grows as it is written.

   type Buffer_Stream is new Root_Stream_Type with
      record
         Data   : Element_Array_Access;
         Length : Stream_Element_Offset := 0;
      end record;

   overriding procedure Read
     (Stream : in out Buffer_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   --  Raise Program_Error: a buffer is only written.

   overriding procedure Write
     (Stream : in out Buffer_Stream;
      Item   : Stream_Element_Array);

   --  A stream that discards what is written, for Caps, which needs only
   --  the names.

   type Discard_Stream is new Root_Stream_Type with null record;

   overriding procedure Read
     (Stream : in out Discard_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Discard_Stream;
      Item   : Stream_Element_Array)
   is null;

   --  The reader of one State: a snapshot, which Release frees.

   type State_Reader is
     new Rose.Interfaces.Releasable_Reader.Server
       .Releasable_Reader_Interface with
      record
         Data     : Element_Array_Access;
         Length   : Stream_Element_Offset := 0;
         Position : Stream_Element_Offset := 0;
      end record;

   type State_Reader_Access is access all State_Reader;

   overriding function Reader_Cap
     (Item : in out State_Reader)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Releasable_Reader.Server.Cap_Of
         (Item, Rose.Interfaces.Releasable_Reader.Reader_Interface));

   overriding function Resource_Cap
     (Item : in out State_Reader)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Releasable_Reader.Server.Cap_Of
         (Item, Rose.Interfaces.Releasable_Reader.Resource_Interface));

   overriding function Releasable_Reader_Cap
     (Item : in out State_Reader)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Releasable_Reader.Server.Cap_Of (Item));

   overriding procedure Read
     (Item   : in out State_Reader;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Release (Item : in out State_Reader);

   --  The Replaceable interface.

   type Replaceable_Server is
     new Rose.Interfaces.Replaceable.Server.Replaceable_Interface
     with null record;

   overriding function Replaceable_Cap
     (Item : in out Replaceable_Server)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Replaceable.Server.Cap_Of (Item));

   overriding function State
     (Item : in out Replaceable_Server)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class;

   overriding function Caps
     (Item : in out Replaceable_Server)
      return Rose.Interfaces.Releasable_Cap_Set.Releasable_Cap_Set_Object'
        Class;

   The_Server     : aliased Replaceable_Server;
   Server_Context : access Rose.Server.Server_Context;
   Writer         : Write_State_Procedure;
   Constructor    : Rose.Interfaces.Constructor.Client.Constructor_Client;
   Attached       : Boolean := False;

   procedure Write_Snapshot
     (Into  : in out Buffer_Stream'Class;
      Named : out Named_Caps);
   --  The state: the header, the identifiers, the named caps and the
   --  server's part.

   ---------
   -- Add --
   ---------

   procedure Add
     (Named : in out Named_Caps;
      Name  : String;
      Cap   : Rose.Capabilities.Capability)
   is
   begin
      Named.Caps.Append
        (Named_Cap'(Ada.Strings.Unbounded.To_Unbounded_String (Name), Cap));
   end Add;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Context         : not null access Rose.Server.Server_Context;
      Write_State     : not null Write_State_Procedure;
      Cap_Set_Service : Rose.Capabilities.Capability)
   is
   begin
      Server_Context := Context;
      Writer := Write_State;
      Constructor.Open (Cap_Set_Service);
      Rose.Interfaces.Replaceable.Server.Attach
        (Context.all, The_Server'Access);
      Attached := True;
   end Attach;

   ---------
   -- Cap --
   ---------

   function Cap
     (Named : Named_Caps;
      Name  : String)
      return Rose.Capabilities.Capability
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Item of Named.Caps loop
         if Item.Name = Name then
            return Item.Cap;
         end if;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Cap;

   ----------
   -- Caps --
   ----------

   overriding function Caps
     (Item : in out Replaceable_Server)
      return Rose.Interfaces.Releasable_Cap_Set.Releasable_Cap_Set_Object'
        Class
   is
      pragma Unreferenced (Item);
      Discard : aliased Discard_Stream;
      Named   : Named_Caps;
      Set_Cap : constant Rose.Capabilities.Capability := Constructor.Create;
      Set     : Rose.Interfaces.Releasable_Cap_Set.Client
        .Releasable_Cap_Set_Client :=
          Rose.Interfaces.Releasable_Cap_Set.Client.From_Cap (Set_Cap);
      Next    : Positive := 1;
   begin
      Writer (Discard'Access, Named);

      --  A call carries at most Max_Capabilities caps.

      while Next <= Named.Caps.Last_Index loop
         declare
            Last  : constant Positive :=
              Positive'Min
                (Named.Caps.Last_Index,
                 Next + Rose.Invocation.Max_Capabilities - 1);
            Batch : Rose.Capabilities.Capability_Array (1 .. Last - Next + 1);
         begin
            for I in Batch'Range loop
               Batch (I) := Named.Caps (Next + I - 1).Cap;
            end loop;
            Set.Append (Batch);
            Next := Last + 1;
         end;
      end loop;

      return Rose.Interfaces.Releasable_Cap_Set.Client.From_Cap (Set_Cap);
   end Caps;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Buffer_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      raise Program_Error with "Rose.Replacement: a buffer is only written";
   end Read;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Discard_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      raise Program_Error with "Rose.Replacement: nothing to read";
   end Read;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out State_Reader;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Count : constant Stream_Element_Offset :=
        Stream_Element_Offset'Min
          (Buffer'Length, Item.Length - Item.Position);
   begin
      if Count > 0 then
         Buffer (Buffer'First .. Buffer'First + Count - 1) :=
           Item.Data (Item.Position + 1 .. Item.Position + Count);
         Item.Position := Item.Position + Count;
      end if;
      Last := Buffer'First + Count - 1;
   end Read;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out State_Reader) is
   begin
      Free (Item.Data);
      Item.Length := 0;
      Item.Position := 0;
   end Release;

   ---------------------
   -- Replaceable_Cap --
   ---------------------

   function Replaceable_Cap return Rose.Capabilities.Capability is
   begin
      if not Attached then
         return Rose.Capabilities.Null_Capability;
      end if;
      return Rose.Interfaces.Replaceable.Server.Cap_Of (The_Server);
   end Replaceable_Cap;

   -----------
   -- State --
   -----------

   overriding function State
     (Item : in out Replaceable_Server)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class
   is
      pragma Unreferenced (Item);
      Snapshot : aliased Buffer_Stream;
      Named    : Named_Caps;
      Reader   : constant State_Reader_Access := new State_Reader;
   begin
      Write_Snapshot (Snapshot, Named);
      Reader.Data := Snapshot.Data;
      Reader.Length := Snapshot.Length;

      --  The reader is attached after the snapshot, so its identifier is
      --  not in the state, and two snapshots of the same state are the
      --  same.

      Rose.Interfaces.Releasable_Reader.Server.Attach
        (Server_Context.all, Reader);
      return Rose.Interfaces.Releasable_Reader.Client.From_Cap
        (Rose.Interfaces.Releasable_Reader.Server.Cap_Of (Reader.all));
   end State;

   ---------------
   -- Take_Over --
   ---------------

   procedure Take_Over
     (Previous   : Rose.Capabilities.Capability;
      Read_State : not null Read_State_Procedure)
   is
   begin
      if Previous = Rose.Capabilities.Null_Capability then
         return;
      end if;

      declare
         Old    : Rose.Interfaces.Replaceable.Client.Replaceable_Client;
      begin
         Old.Open (Previous);

         declare
            Reader : aliased Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := Old.State;
            Set    : Rose.Interfaces.Releasable_Cap_Set
              .Releasable_Cap_Set_Object'Class := Old.Caps;
            Input  : aliased Rose.Streams.Reader_Stream (Reader'Access);
            Header : Rose.Words.Word_32;
            Format : Rose.Words.Word_32;
            Count  : Natural;
            Named  : Named_Caps;
         begin
            Rose.Words.Word_32'Read (Input'Access, Header);
            Rose.Words.Word_32'Read (Input'Access, Format);
            if Header /= Magic or else Format /= Version then
               raise Bad_State with
                 "Rose.Replacement: not a state that it can read";
            end if;

            Rose.Server.Read_Identifiers (Input'Access);

            Natural'Read (Input'Access, Count);
            for I in 1 .. Count loop
               declare
                  Name  : constant String := String'Input (Input'Access);
                  Index : Positive;
               begin
                  Positive'Read (Input'Access, Index);
                  Named.Add (Name, Set.Get_Cap (Index));
               end;
            end loop;

            Read_State (Input'Access, Named);

            Reader.Release;
            Set.Release;
         end;
      end;
   end Take_Over;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Buffer_Stream;
      Item   : Stream_Element_Array)
   is
      Needed : constant Stream_Element_Offset := Stream.Length + Item'Length;
   begin
      if Stream.Data = null then
         Stream.Data :=
           new Stream_Element_Array
             (1 .. Stream_Element_Offset'Max (4096, Needed));
      elsif Needed > Stream.Data'Length then
         declare
            Grown : constant Element_Array_Access :=
              new Stream_Element_Array
                (1 .. Stream_Element_Offset'Max
                        (2 * Stream.Data'Length, Needed));
         begin
            Grown (1 .. Stream.Length) := Stream.Data (1 .. Stream.Length);
            Free (Stream.Data);
            Stream.Data := Grown;
         end;
      end if;

      Stream.Data (Stream.Length + 1 .. Needed) := Item;
      Stream.Length := Needed;
   end Write;

   --------------------
   -- Write_Snapshot --
   --------------------

   procedure Write_Snapshot
     (Into  : in out Buffer_Stream'Class;
      Named : out Named_Caps)
   is
      Server_Part : aliased Buffer_Stream;
   begin
      Writer (Server_Part'Access, Named);

      Rose.Words.Word_32'Write (Into'Access, Magic);
      Rose.Words.Word_32'Write (Into'Access, Version);
      Rose.Server.Write_Identifiers (Into'Access);

      Natural'Write (Into'Access, Natural (Named.Caps.Length));
      for Index in 1 .. Named.Caps.Last_Index loop
         String'Output
           (Into'Access,
            Ada.Strings.Unbounded.To_String (Named.Caps (Index).Name));
         Positive'Write (Into'Access, Index);
      end loop;

      if Server_Part.Length > 0 then
         Into.Write (Server_Part.Data (1 .. Server_Part.Length));
      end if;
      Free (Server_Part.Data);
   end Write_Snapshot;

end Rose.Replacement;
