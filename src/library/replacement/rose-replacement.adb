with Ada.Unchecked_Deallocation;

with Rose.Interfaces.Constructor.Client;
with Rose.Interfaces.Releasable_Cap_Set.Client;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Releasable_Reader.Server;
with Rose.Interfaces.Reader.Client;
with Rose.Interfaces.Replaceable.Client;
with Rose.Interfaces.Replaceable.Server;
with Rose.Interfaces.Writer.Client;
with Rose.Invocation;
with Rose.Objects;
with Rose.Streams;
with Rose.System_Calls;
with Rose.Words;

package body Rose.Replacement is

   use Ada.Streams;
   use type Rose.Capabilities.Capability;
   use type Rose.Objects.Capability_Identifier;
   use type Rose.Words.Word_32;

   Magic   : constant Rose.Words.Word_32 := 16#5245_504C#;
   Version : constant Rose.Words.Word_32 := 2;
   --  The header of a state, and the version of this package's part.
   --  Version 2 has the deferred calls (#879).

   type Element_Array_Access is access Stream_Element_Array;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Stream_Element_Array, Element_Array_Access);

   package Call_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rose.Invocation.Invocation_Record, Rose.Invocation."=");

   package Cap_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rose.Capabilities.Capability, Rose.Capabilities."=");

   package Identifier_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rose.Objects.Capability_Identifier, Rose.Objects."=");

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
         Data       : Element_Array_Access;
         Length     : Stream_Element_Offset := 0;
         Position   : Stream_Element_Offset := 0;
         Identifier : Rose.Objects.Capability_Identifier := 0;
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

   overriding function Saveable_Cap
     (Item : in out Replaceable_Server)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Replaceable.Server.Cap_Of
         (Item, Rose.Interfaces.Replaceable.Saveable_Interface));

   overriding function State
     (Item : in out Replaceable_Server)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class;

   overriding function Caps
     (Item : in out Replaceable_Server)
      return Rose.Interfaces.Releasable_Cap_Set.Releasable_Cap_Set_Object'
        Class;

   overriding function Replies
     (Item : in out Replaceable_Server)
      return Rose.Interfaces.Releasable_Cap_Set.Releasable_Cap_Set_Object'
        Class;

   The_Server     : aliased Replaceable_Server;
   Server_Context : access Rose.Server.Server_Context;
   Writer         : Write_State_Procedure;
   Constructor    : Rose.Interfaces.Constructor.Client.Constructor_Client;
   Attached       : Boolean := False;

   Reader_Ids : Identifier_Vectors.Vector;
   --  The identifiers of the readers of State that are not released.

   Endpoint_Reader : aliased State_Reader;
   --  A reader that is attached with the Replaceable interface, and never
   --  given out. The first reader that is attached registers the reader's
   --  endpoints, so without it, a version that has served State would
   --  have endpoints that a new version, which hasn't, lacks, and the
   --  kernel would refuse to swap them (#866).

   --  The reply caps of the deferred calls (#879). In the old version,
   --  Replies_Given is set once Replies has moved them into Replies_Set,
   --  and Given_Slots holds the slot that each one left, by its index in
   --  the set. In the new version, Replies_Taken is set once Take_Over has
   --  read the deferred calls, which are Taken_Calls, each at its index,
   --  and has Replies_Set.

   Replies_Set   : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Replies_Given : Boolean := False;
   Replies_Taken : Boolean := False;
   Given_Slots   : Cap_Vectors.Vector;
   Taken_Calls   : Call_Vectors.Vector;

   Launch_Arguments : Named_Caps;
   --  The command's cap arguments, which Start was given. State, Save and
   --  Caps hand them over after the caps that the server names, so that
   --  a new version can take any that its caller doesn't give (#901).

   --  A handover that Arguments has begun, and Take_Over finishes: the
   --  old version's state, read up to the server's part, and its caps.

   Pending          : Boolean := False;
   Pending_Previous : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Pending_Reader   : aliased Rose.Interfaces.Releasable_Reader.Client
     .Releasable_Reader_Client;
   Pending_Input    : aliased Rose.Streams.Reader_Stream
     (Pending_Reader'Access);
   Pending_Set      : aliased Rose.Interfaces.Releasable_Cap_Set.Client
     .Releasable_Cap_Set_Client;
   Pending_Named    : Named_Caps;
   Pending_Calls    : Call_Vectors.Vector;

   procedure Write_Snapshot
     (Into       : in out Buffer_Stream'Class;
      Named      : out Named_Caps;
      With_Calls : Boolean);
   --  The state: the header, the identifiers, the named caps, the
   --  deferred calls and the server's part. The list of deferred calls is
   --  empty unless With_Calls: only a handover can move them, so a state
   --  that Save writes has none (section 10).

   procedure Read_Prefix
     (Input : not null access Root_Stream_Type'Class;
      Set   : access Rose.Interfaces.Releasable_Cap_Set
                .Releasable_Cap_Set_Object'Class;
      Named   : in out Named_Caps;
      Calls   : out Call_Vectors.Vector;
      Movable : out Boolean);
   --  Read this package's part of a state, from Input: check its header,
   --  mark its identifiers in use, read the names of its caps, and read
   --  its deferred calls into Calls. If Set is not null, add each cap to
   --  Named, with its cap in Set; if it is null, skip them, as a restore
   --  does. Movable is False if a deferred call can't move (Can_Move).

   procedure Abandon_Take_Over;
   --  Release the old version's state and caps, if a handover is pending.

   procedure Add_Arguments (Named : in out Named_Caps);
   --  Add each of Launch_Arguments that is a cap, and whose name Named
   --  doesn't have a cap for already.

   procedure Begin_Take_Over (Previous : Rose.Capabilities.Capability);
   --  Open the state and the caps of Previous, the old version's
   --  Replaceable interface, and read the state up to the server's part,
   --  into the pending handover.

   function Deferred_Calls return Call_Vectors.Vector;
   --  The calls that the server has deferred with a reply cap, in the
   --  order of their handlers, which is the order of their reply caps in
   --  the set that Replies gives.

   function Can_Move
     (Call : Rose.Invocation.Invocation_Record)
      return Boolean;
   --  Call can be answered by the new version: it has no buffer, which
   --  is mapped into the old version only (#878), and no caps.

   function Is_Handover_Call
     (Identifier : Rose.Objects.Capability_Identifier)
      return Boolean;
   --  A call to Identifier is one that the handover makes: to
   --  Replaceable, or to a reader of State.

   procedure Before_Call (Params : Rose.Invocation.Invocation_Record);
   --  The receive hook: before the server serves a call that the
   --  handover doesn't make, take any reply caps that are in Replies_Set.

   procedure Take_Back;
   --  In the old version, after a handover failed: put each reply cap
   --  back in its deferred call, and forget a call whose cap is gone.

   procedure Take_Replies;
   --  In the new version, after it has taken over: defer each of the old
   --  version's calls again, with its reply cap.

   function Taken
     (Set   : in out Rose.Interfaces.Releasable_Cap_Set
        .Releasable_Cap_Set_Object'Class;
      Index : Positive)
      return Rose.Capabilities.Capability;
   --  The cap at Index in Set, which moves it out of the set if it is a
   --  reply cap, or the null cap if it is gone, or Set is.

   procedure Forget_Replies;
   --  Release Replies_Set, and forget it.

   -----------------------
   -- Abandon_Take_Over --
   -----------------------

   procedure Abandon_Take_Over is
   begin
      if not Pending then
         return;
      end if;

      Pending := False;
      begin
         Pending_Reader.Release;
      exception
         when others =>
            null;
      end;
      begin
         Pending_Set.Release;
      exception
         when others =>
            null;
      end;
   end Abandon_Take_Over;

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

   -------------------
   -- Add_Arguments --
   -------------------

   procedure Add_Arguments (Named : in out Named_Caps) is
   begin
      for Item of Launch_Arguments.Caps loop
         if Item.Cap /= Rose.Capabilities.Null_Capability
           and then Named.Cap (Ada.Strings.Unbounded.To_String (Item.Name))
             = Rose.Capabilities.Null_Capability
         then
            Named.Caps.Append (Item);
         end if;
      end loop;
   end Add_Arguments;

   ---------------
   -- Arguments --
   ---------------

   function Arguments
     (Previous : Rose.Capabilities.Capability;
      Given    : Named_Caps)
      return Named_Caps
   is
      Result : Named_Caps := Given;
   begin
      if Previous = Rose.Capabilities.Null_Capability then
         return Result;
      end if;

      if not Pending or else Pending_Previous /= Previous then
         Abandon_Take_Over;
         Begin_Take_Over (Previous);
      end if;

      for Item of Result.Caps loop
         if Item.Cap = Rose.Capabilities.Null_Capability then
            Item.Cap :=
              Pending_Named.Cap (Ada.Strings.Unbounded.To_String (Item.Name));
         end if;
      end loop;

      return Result;
   end Arguments;

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
      Rose.Interfaces.Releasable_Reader.Server.Attach
        (Context.all, Endpoint_Reader'Access);
      Rose.Server.Set_Receive_Hook (Context.all, Before_Call'Access);
      Attached := True;
   end Attach;

   -----------------
   -- Before_Call --
   -----------------

   procedure Before_Call (Params : Rose.Invocation.Invocation_Record) is
   begin
      if Is_Handover_Call (Params.Identifier) then
         return;
      end if;

      if Replies_Given then
         Take_Back;
      end if;

      if Replies_Taken then
         Take_Replies;
      end if;

   exception
      when others =>

         --  Taken and Forget_Replies handle a set that is gone, so this
         --  is a failure of the heap. The server goes on, without the
         --  reply caps that it didn't get.

         Replies_Given := False;
         Replies_Taken := False;
         Replies_Set := Rose.Capabilities.Null_Capability;
   end Before_Call;

   ---------------------
   -- Begin_Take_Over --
   ---------------------

   procedure Begin_Take_Over (Previous : Rose.Capabilities.Capability) is
      Old     : Rose.Interfaces.Replaceable.Client.Replaceable_Client;
      Movable : Boolean;
   begin
      Old.Open (Previous);

      declare
         Reader : Rose.Interfaces.Releasable_Reader
           .Releasable_Reader_Object'Class := Old.State;
      begin
         Pending_Reader.Open (Reader.Releasable_Reader_Cap);
      end;

      Pending := True;
      Pending_Previous := Previous;

      declare
         Set : Rose.Interfaces.Releasable_Cap_Set
           .Releasable_Cap_Set_Object'Class := Old.Caps;
      begin
         Pending_Set.Open (Set.Releasable_Cap_Set_Cap);
      end;

      Pending_Named.Caps.Clear;
      Read_Prefix
        (Pending_Input'Access, Pending_Set'Access, Pending_Named,
         Pending_Calls, Movable);
      if not Movable then
         raise Bad_State with
           "Rose.Replacement: a deferred call has a buffer or caps";
      end if;

      --  The reply caps are taken from the set only once this version
      --  serves a call, which is after it has taken over. Until then they
      --  stay in the set, where the old version takes them back if the
      --  handover fails.

      if not Pending_Calls.Is_Empty then
         declare
            Replies : Rose.Interfaces.Releasable_Cap_Set
              .Releasable_Cap_Set_Object'Class := Old.Replies;
         begin
            Replies_Set := Replies.Releasable_Cap_Set_Cap;
         end;
      end if;

   exception
      when others =>
         Abandon_Take_Over;
         raise;
   end Begin_Take_Over;

   --------------
   -- Can_Move --
   --------------

   function Can_Move
     (Call : Rose.Invocation.Invocation_Record)
      return Boolean
   is
      use Rose.Invocation;
   begin
      if Call.Control.Flags (Send_Buffer)
        or else Call.Control.Flags (Writable_Buffer)
      then
         return False;
      end if;

      if Call.Control.Flags (Send_Caps) then
         for Index in 0 .. Call.Control.Last_Sent_Cap loop
            if Call.Caps (Index) /= Rose.Capabilities.Null_Capability then
               return False;
            end if;
         end loop;
      end if;

      return True;
   end Can_Move;

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
      Add_Arguments (Named);

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

   --------------------
   -- Deferred_Calls --
   --------------------

   function Deferred_Calls return Call_Vectors.Vector is
      Calls : Call_Vectors.Vector;

      procedure Collect
        (Params : in out Rose.Invocation.Invocation_Record;
         Keep   : in out Boolean);

      -------------
      -- Collect --
      -------------

      procedure Collect
        (Params : in out Rose.Invocation.Invocation_Record;
         Keep   : in out Boolean)
      is
         pragma Unreferenced (Keep);
      begin
         if Params.Reply_Cap /= Rose.Capabilities.Null_Capability then
            Calls.Append (Params);
         end if;
      end Collect;

   begin
      Rose.Server.Update_Deferred_Calls
        (Server_Context.all, Collect'Access);
      return Calls;
   end Deferred_Calls;

   --------------------
   -- Forget_Replies --
   --------------------

   procedure Forget_Replies is
   begin
      declare
         Set : Rose.Interfaces.Releasable_Cap_Set.Client
           .Releasable_Cap_Set_Client :=
             Rose.Interfaces.Releasable_Cap_Set.Client.From_Cap
               (Replies_Set);
      begin
         Set.Release;
      end;
      Replies_Set := Rose.Capabilities.Null_Capability;
   exception
      when others =>

         --  The set was released already: its reply caps are gone.

         Replies_Set := Rose.Capabilities.Null_Capability;
   end Forget_Replies;

   ----------------------
   -- Is_Handover_Call --
   ----------------------

   function Is_Handover_Call
     (Identifier : Rose.Objects.Capability_Identifier)
      return Boolean
   is
   begin
      return Identifier =
        Rose.Interfaces.Replaceable.Server.Identifier_Of (The_Server'Access)
        or else Reader_Ids.Contains (Identifier);
   end Is_Handover_Call;

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

   -----------------
   -- Read_Prefix --
   -----------------

   procedure Read_Prefix
     (Input : not null access Root_Stream_Type'Class;
      Set   : access Rose.Interfaces.Releasable_Cap_Set
                .Releasable_Cap_Set_Object'Class;
      Named   : in out Named_Caps;
      Calls   : out Call_Vectors.Vector;
      Movable : out Boolean)
   is
      Header : Rose.Words.Word_32;
      Format : Rose.Words.Word_32;
      Count  : Natural;
   begin
      Calls.Clear;
      Movable := True;

      Rose.Words.Word_32'Read (Input, Header);
      Rose.Words.Word_32'Read (Input, Format);
      if Header /= Magic or else Format /= Version then
         raise Bad_State with
           "Rose.Replacement: not a state that it can read";
      end if;

      Rose.Server.Read_Identifiers (Input);

      Natural'Read (Input, Count);
      for I in 1 .. Count loop
         declare
            Name  : constant String := String'Input (Input);
            Index : Positive;
         begin
            Positive'Read (Input, Index);
            if Set /= null then
               Named.Add (Name, Set.Get_Cap (Index));
            end if;
         end;
      end loop;

      Natural'Read (Input, Count);
      for I in 1 .. Count loop
         declare
            Endpoint   : Rose.Objects.Endpoint_Id;
            Identifier : Rose.Objects.Capability_Identifier;
            Control    : Rose.Invocation.Control_Word;
            Data       : Rose.Invocation.Parameter_Words;
            Can_Go     : Boolean;
            Call       : Rose.Invocation.Invocation_Record;
         begin
            Rose.Objects.Endpoint_Id'Read (Input, Endpoint);
            Rose.Objects.Capability_Identifier'Read (Input, Identifier);
            Rose.Invocation.Control_Word'Read (Input, Control);
            Rose.Invocation.Parameter_Words'Read (Input, Data);
            Boolean'Read (Input, Can_Go);
            Movable := Movable and then Can_Go;
            Call.Control := Control;
            Call.Endpoint := Endpoint;
            Call.Identifier := Identifier;
            Call.Data := Data;
            Calls.Append (Call);
         end;
      end loop;
   end Read_Prefix;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out State_Reader) is
      Index : constant Identifier_Vectors.Extended_Index :=
        Reader_Ids.Find_Index (Item.Identifier);
   begin
      Free (Item.Data);
      Item.Length := 0;
      Item.Position := 0;
      if Index /= Identifier_Vectors.No_Index then
         Reader_Ids.Delete (Index);
      end if;
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

   -------------
   -- Replies --
   -------------

   overriding function Replies
     (Item : in out Replaceable_Server)
      return Rose.Interfaces.Releasable_Cap_Set.Releasable_Cap_Set_Object'
        Class
   is
      pragma Unreferenced (Item);
      Calls   : constant Call_Vectors.Vector := Deferred_Calls;
      Set_Cap : Rose.Capabilities.Capability;
      Next    : Positive := 1;
   begin
      if Replies_Given then
         raise Constraint_Error with
           "Rose.Replacement: the reply caps have been given out";
      end if;

      Set_Cap := Constructor.Create;

      declare
         Set : Rose.Interfaces.Releasable_Cap_Set.Client
           .Releasable_Cap_Set_Client :=
             Rose.Interfaces.Releasable_Cap_Set.Client.From_Cap (Set_Cap);
      begin
         --  Sending a reply cap moves it (#864): its slot here is left
         --  empty, and kept, until Take_Back deletes it.

         while Next <= Calls.Last_Index loop
            declare
               Last  : constant Positive :=
                 Positive'Min
                   (Calls.Last_Index,
                    Next + Rose.Invocation.Max_Capabilities - 1);
               Batch : Rose.Capabilities.Capability_Array
                 (1 .. Last - Next + 1);
            begin
               for I in Batch'Range loop
                  Batch (I) := Calls (Next + I - 1).Reply_Cap;
               end loop;
               Set.Append (Batch);
               Next := Last + 1;
            end;
         end loop;
      end;

      Given_Slots.Clear;
      for Call of Calls loop
         Given_Slots.Append (Call.Reply_Cap);
      end loop;
      Replies_Set := Set_Cap;
      Replies_Given := True;

      return Rose.Interfaces.Releasable_Cap_Set.Client.From_Cap (Set_Cap);
   end Replies;

   -------------
   -- Restore --
   -------------

   procedure Restore
     (Saved      : Rose.Capabilities.Capability;
      Arguments  : Named_Caps;
      Read_State : not null Read_State_Procedure)
   is
   begin
      if Saved = Rose.Capabilities.Null_Capability then
         return;
      end if;

      --  A saved state's deferred calls, if a State listed any, are
      --  ignored: their replies can only move in a handover.

      declare
         Reader : aliased Rose.Interfaces.Reader.Client.Reader_Client :=
           Rose.Interfaces.Reader.Client.From_Cap (Saved);
         Input  : aliased Rose.Streams.Reader_Stream (Reader'Access);
         Unused  : Named_Caps;
         Calls   : Call_Vectors.Vector;
         Movable : Boolean;
      begin
         Read_Prefix (Input'Access, null, Unused, Calls, Movable);
         Read_State (Input'Access, Arguments);
      end;
   end Restore;

   ----------
   -- Save --
   ----------

   procedure Save (To : Rose.Capabilities.Capability) is
      Snapshot : aliased Buffer_Stream;
      Named    : Named_Caps;
      Writer   : aliased Rose.Interfaces.Writer.Client.Writer_Client :=
        Rose.Interfaces.Writer.Client.From_Cap (To);
      Output   : aliased Rose.Streams.Writer_Stream (Writer'Access);
   begin
      if not Attached then
         raise Program_Error with "Rose.Replacement: Save before Attach";
      end if;
      Write_Snapshot (Snapshot, Named, With_Calls => False);
      if Snapshot.Length > 0 then
         Output.Write (Snapshot.Data (1 .. Snapshot.Length));
      end if;
      Free (Snapshot.Data);
   end Save;

   ------------------
   -- Saveable_Cap --
   ------------------

   function Saveable_Cap return Rose.Capabilities.Capability is
   begin
      if not Attached then
         return Rose.Capabilities.Null_Capability;
      end if;
      return Rose.Interfaces.Replaceable.Server.Cap_Of
        (The_Server, Rose.Interfaces.Replaceable.Saveable_Interface);
   end Saveable_Cap;

   -----------
   -- Start --
   -----------

   procedure Start
     (Previous   : Rose.Capabilities.Capability;
      Saved      : Rose.Capabilities.Capability;
      Arguments  : Named_Caps;
      Read_State : not null Read_State_Procedure)
   is
   begin
      if Previous /= Rose.Capabilities.Null_Capability
        and then Saved /= Rose.Capabilities.Null_Capability
      then
         raise Bad_Start with
           "Rose.Replacement: both a version to take over and a saved state";
      end if;

      Launch_Arguments := Arguments;

      if Previous /= Rose.Capabilities.Null_Capability then
         Take_Over (Previous, Read_State);
      else
         Restore (Saved, Arguments, Read_State);
      end if;
   end Start;

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
      Write_Snapshot (Snapshot, Named, With_Calls => True);
      Reader.Data := Snapshot.Data;
      Reader.Length := Snapshot.Length;

      --  The reader is attached after the snapshot, so its identifier is
      --  not in the state, and two snapshots of the same state are the
      --  same.

      Rose.Interfaces.Releasable_Reader.Server.Attach
        (Server_Context.all, Reader);
      Reader.Identifier :=
        Rose.Interfaces.Releasable_Reader.Server.Identifier_Of (Reader);
      Reader_Ids.Append (Reader.Identifier);
      return Rose.Interfaces.Releasable_Reader.Client.From_Cap
        (Rose.Interfaces.Releasable_Reader.Server.Cap_Of (Reader.all));
   end State;

   ---------------
   -- Take_Back --
   ---------------

   procedure Take_Back is
      Set : Rose.Interfaces.Releasable_Cap_Set.Client
        .Releasable_Cap_Set_Client :=
          Rose.Interfaces.Releasable_Cap_Set.Client.From_Cap (Replies_Set);

      procedure Put_Back
        (Params : in out Rose.Invocation.Invocation_Record;
         Keep   : in out Boolean);

      --------------
      -- Put_Back --
      --------------

      procedure Put_Back
        (Params : in out Rose.Invocation.Invocation_Record;
         Keep   : in out Boolean)
      is
         Index : constant Cap_Vectors.Extended_Index :=
           Given_Slots.Find_Index (Params.Reply_Cap);
      begin
         if Index = Cap_Vectors.No_Index then
            return;
         end if;

         declare
            Reply_Cap : constant Rose.Capabilities.Capability :=
              Taken (Set, Index);
         begin
            Rose.System_Calls.Delete_Cap (Params.Reply_Cap);
            if Reply_Cap = Rose.Capabilities.Null_Capability then
               Keep := False;
            else
               Params.Reply_Cap := Reply_Cap;
            end if;
         end;
      end Put_Back;

   begin
      Rose.Server.Update_Deferred_Calls
        (Server_Context.all, Put_Back'Access);
      Forget_Replies;
      Given_Slots.Clear;
      Replies_Given := False;
   end Take_Back;

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

      --  The main has begun the handover already if it called Arguments.

      if not Pending or else Pending_Previous /= Previous then
         Abandon_Take_Over;
         Begin_Take_Over (Previous);
      end if;

      Read_State (Pending_Input'Access, Pending_Named);

      Pending := False;
      Pending_Reader.Release;
      Pending_Set.Release;

      Taken_Calls := Pending_Calls;
      Replies_Taken := not Pending_Calls.Is_Empty;

   exception
      when others =>
         Abandon_Take_Over;
         raise;
   end Take_Over;

   ------------------
   -- Take_Replies --
   ------------------

   procedure Take_Replies is
      Set : Rose.Interfaces.Releasable_Cap_Set.Client
        .Releasable_Cap_Set_Client :=
          Rose.Interfaces.Releasable_Cap_Set.Client.From_Cap (Replies_Set);
   begin
      for Index in 1 .. Taken_Calls.Last_Index loop
         declare
            Reply_Cap : constant Rose.Capabilities.Capability :=
              Taken (Set, Index);
            Call      : aliased Rose.Invocation.Invocation_Record :=
              Taken_Calls (Index);
            Deferred  : Boolean;
         begin
            if Reply_Cap /= Rose.Capabilities.Null_Capability then
               Call.Reply_Cap := Reply_Cap;
               Rose.Server.Defer_Call (Server_Context.all, Call, Deferred);

               --  A call that this version doesn't serve fails, as one
               --  that no handler takes does in Rose.Server.

               if not Deferred then
                  Rose.System_Calls.Initialize_Reply (Call, Reply_Cap);
                  Rose.System_Calls.Send_Error
                    (Call, Rose.Invocation.Invalid_Endpoint);
                  Rose.System_Calls.Invoke_Capability (Call);
               end if;
            end if;
         end;
      end loop;

      Forget_Replies;
      Taken_Calls.Clear;
      Replies_Taken := False;
   end Take_Replies;

   -----------
   -- Taken --
   -----------

   function Taken
     (Set   : in out Rose.Interfaces.Releasable_Cap_Set
        .Releasable_Cap_Set_Object'Class;
      Index : Positive)
      return Rose.Capabilities.Capability
   is
   begin
      return Set.Get_Cap (Index);
   exception
      when others =>
         return Rose.Capabilities.Null_Capability;
   end Taken;

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
     (Into       : in out Buffer_Stream'Class;
      Named      : out Named_Caps;
      With_Calls : Boolean)
   is
      Server_Part : aliased Buffer_Stream;
      Calls       : constant Call_Vectors.Vector :=
        (if With_Calls then Deferred_Calls else Call_Vectors.Empty_Vector);
   begin
      Writer (Server_Part'Access, Named);
      Add_Arguments (Named);

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

      --  Each deferred call is at its index in the order of Calls, which
      --  is the order of their reply caps in the set that Replies gives.

      Natural'Write (Into'Access, Natural (Calls.Length));
      for Call of Calls loop
         Rose.Objects.Endpoint_Id'Write (Into'Access, Call.Endpoint);
         Rose.Objects.Capability_Identifier'Write
           (Into'Access, Call.Identifier);
         Rose.Invocation.Control_Word'Write (Into'Access, Call.Control);
         Rose.Invocation.Parameter_Words'Write (Into'Access, Call.Data);
         Boolean'Write (Into'Access, Can_Move (Call));
      end loop;

      if Server_Part.Length > 0 then
         Into.Write (Server_Part.Data (1 .. Server_Part.Length));
      end if;
      Free (Server_Part.Data);
   end Write_Snapshot;

end Rose.Replacement;
