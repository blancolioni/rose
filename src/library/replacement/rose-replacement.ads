with Ada.Streams;

with Rose.Capabilities;
with Rose.Server;

private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package Rose.Replacement is

   --  A server that a new version of its program can take over from
   --  (ADR 0032, #865). The old version answers the Replaceable interface,
   --  and the new version is launched with that interface as its
   --  Previous argument. A command with idle's Replaceable aspect does
   --  both: its server package attaches the interface with Attach, and
   --  its generated main calls Take_Over before the command.
   --
   --  The state is a stream: this package's header, the identifiers in
   --  use (Rose.Server.Write_Identifiers), the server's named caps, each
   --  with its index in the cap set that Caps returns, the calls that the
   --  server has deferred (Rose.Server.Block_Current_Request), each with
   --  its index in the set that Replies returns, and then whatever the
   --  server writes, which starts with its own format version. A server
   --  writes and reads only its part, with stream attributes, and names
   --  the caps it holds.
   --
   --  The deferred calls move to the new version with nothing for the
   --  server to do (#879). Their reply caps can be in only one place, so
   --  Replies gives them out once, in a set that the old version keeps a
   --  cap to. Whichever version next serves a call takes them from the
   --  set: the new version, once it has taken over, which defers each
   --  call again with its reply cap, or the old version, if the handover
   --  failed, which puts each back. Calls on Replaceable, and on the
   --  readers of State, don't count, since the handover makes them. A
   --  signal does count, so one that reaches the old version during a
   --  handover takes the reply caps back, and the new version then has
   --  none: those calls fail when the old version ends.
   --
   --  Only replaceable servers list this package's directory in their
   --  project, because it needs their generated packages of Replaceable,
   --  Releasable_Reader and its parents, Releasable_Cap_Set and its
   --  parents, and Constructor.

   type Named_Caps is tagged private;
   --  The caps that a server holds, each with a name.

   procedure Add
     (Named : in out Named_Caps;
      Name  : String;
      Cap   : Rose.Capabilities.Capability);
   --  Name Cap. A name is given once.

   function Cap
     (Named : Named_Caps;
      Name  : String)
      return Rose.Capabilities.Capability;
   --  The cap called Name, or the null cap if there is none.

   type Write_State_Procedure is access procedure
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Named_Caps);
   --  Write the server's state to Stream, starting with its format
   --  version, and add each cap that it holds to Named. It is called for
   --  each call of State, and again for each call of Caps, so it must add
   --  the same caps in the same order each time, and change nothing.

   type Read_State_Procedure is access procedure
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Named_Caps);
   --  Read what Write_State wrote, from the old version, taking the caps
   --  it needs from Named. It raises if it can't read the format, or a
   --  cap it needs is missing, and the new version then fails its launch.

   procedure Attach
     (Context         : not null access Rose.Server.Server_Context;
      Write_State     : not null Write_State_Procedure;
      Cap_Set_Service : Rose.Capabilities.Capability);
   --  Answer Replaceable in Context, which the server serves: State with
   --  what Write_State writes, Caps with what it names, and Replies with
   --  the reply caps of Context's deferred calls, each in a new set from
   --  Cap_Set_Service, the cap set server's Constructor. Context is kept,
   --  to attach the reader of each State to, and to take the reply caps
   --  in before it serves a call.

   function Replaceable_Cap return Rose.Capabilities.Capability;
   --  The cap of the Replaceable interface, once it is attached.

   procedure Take_Over
     (Previous   : Rose.Capabilities.Capability;
      Read_State : not null Read_State_Procedure);
   --  If Previous is a cap, the old version's Replaceable interface: read
   --  its state and caps, mark its identifiers in use, so that the new
   --  version's objects can be given them, call Read_State with the
   --  server's part, and release the state and the set of caps. If the
   --  old version has deferred calls, also take the set of their replies,
   --  whose caps the new version takes before it serves its first call.
   --  Nothing is done for the null cap, which is a fresh start.

   Bad_State : exception;
   --  The state is not one that this package wrote, or it has a deferred
   --  call that can't move: one with a buffer, which was mapped into the
   --  old version only (#878), or with caps.

private

   type Named_Cap is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Cap  : Rose.Capabilities.Capability;
      end record;

   package Named_Cap_Vectors is
     new Ada.Containers.Vectors (Positive, Named_Cap);

   type Named_Caps is tagged
      record
         Caps : Named_Cap_Vectors.Vector;
      end record;

end Rose.Replacement;
