with Ada.Containers.Vectors;
with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Server;
with Rose.Standard_Streams;
with Rose.System_Calls;

with Rose.Interfaces.Constructor.Server;
with Rose.Interfaces.Releasable_Cap_Set.Server;
with Rose.Interfaces.Restorable.Server;

package body Caps.Server is

   use type Rose.Capabilities.Capability;

   --  Every cap in every set is a cap in this process's table, which holds
   --  at most 4095 (Rose.Kernel.Cap_Tables). That is the limit on what the
   --  sets hold together; a set has no limit of its own (#825).

   Table_Size   : constant := 4095;

   Reserve      : constant := 128;
   --  Kept for the server's own caps, and the reply caps of the calls that
   --  it is answering.

   Set_Overhead : constant := 7;
   --  The caps that the server holds for a set: the set's own, its four
   --  operations and its two ancestors (the generated server makes the
   --  last six when they are asked for).

   Limit        : constant := Table_Size - Reserve;

   Used         : Natural := 0;
   --  The caps held for sets: their contents, and Set_Overhead each.

   Set_Count    : Natural := 0;

   Cap_Set_Full : exception;
   No_Such_Cap  : exception;

   package Cap_Vectors is
     new Ada.Containers.Vectors (Positive, Rose.Capabilities.Capability,
                                 Rose.Capabilities."=");

   type Set_Object;
   type Set_Access is access all Set_Object;

   type Set_Object is
     new Rose.Interfaces.Releasable_Cap_Set.Server
       .Releasable_Cap_Set_Interface with
      record
         Active : Boolean := False;
         Caps   : Cap_Vectors.Vector;
         Next   : Set_Access;
      end record;

   overriding function Releasable_Cap_Set_Cap
     (Item : in out Set_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Releasable_Cap_Set.Server.Cap_Of (Item));

   overriding function Cap_Set_Cap
     (Item : in out Set_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Releasable_Cap_Set.Server.Cap_Of
         (Item, Rose.Interfaces.Releasable_Cap_Set.Cap_Set_Interface));

   overriding function Resource_Cap
     (Item : in out Set_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Releasable_Cap_Set.Server.Cap_Of
         (Item, Rose.Interfaces.Releasable_Cap_Set.Resource_Interface));

   overriding procedure Append
     (Item : in out Set_Object;
      Caps : Rose.Capabilities.Capability_Array);

   overriding function Get_Cap
     (Item  : in out Set_Object;
      Index : Positive)
      return Rose.Capabilities.Capability;

   overriding function Length
     (Item : in out Set_Object)
      return Natural
   is (Natural (Item.Caps.Length));

   overriding procedure Release (Item : in out Set_Object);

   --  One object makes the sets, and takes the restore notice.

   type Constructor_Object is
     new Rose.Interfaces.Constructor.Server.Constructor_Interface
     and Rose.Interfaces.Restorable.Server.Restorable_Interface
     with null record;

   overriding function Constructor_Cap
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Constructor.Server.Cap_Of (Item));

   overriding function Restorable_Cap
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Restorable.Server.Cap_Of (Item));

   overriding function Create
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability;

   overriding procedure Restored (Item : in out Constructor_Object);

   Context         : Rose.Server.Server_Context;
   The_Constructor : aliased Constructor_Object;

   Sets            : Set_Access;
   --  Every set that has been made. A released set is kept, inactive,
   --  for Create to use again.

   function Image (X : Natural) return String;
   --  X, with no leading space.

   procedure Open_Console;
   --  Write to the console's Writer, if the installer gave one.

   ------------
   -- Append --
   ------------

   --  A set that can't take every cap takes none of them, and raises. The
   --  caps have arrived in this process's table already, and the
   --  generated handler deletes them when the operation raises
   --  (idle#95), so they must not be kept or deleted here.

   overriding procedure Append
     (Item : in out Set_Object;
      Caps : Rose.Capabilities.Capability_Array)
   is
   begin
      if Caps'Length > Limit - Used then
         raise Cap_Set_Full with
           "the cap sets are full: " & Image (Used) & " caps are held";
      end if;

      for Cap of Caps loop
         Item.Caps.Append (Cap);
      end loop;
      Used := Used + Caps'Length;
   end Append;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Item);
      Set : Set_Access := Sets;
   begin
      if Set_Overhead > Limit - Used then
         raise Cap_Set_Full with
           "no room for another cap set: " & Image (Used)
           & " caps are held";
      end if;

      while Set /= null and then Set.Active loop
         Set := Set.Next;
      end loop;

      if Set = null then
         Set := new Set_Object;
         Set.Next := Sets;
         Sets := Set;
      end if;

      Set.Active := True;
      Set.Caps.Clear;
      Rose.Interfaces.Releasable_Cap_Set.Server.Attach (Context, Set);
      Used := Used + Set_Overhead;
      Set_Count := Set_Count + 1;
      return Rose.Interfaces.Releasable_Cap_Set.Server.Interface_Cap (Set);
   end Create;

   -------------
   -- Get_Cap --
   -------------

   overriding function Get_Cap
     (Item  : in out Set_Object;
      Index : Positive)
      return Rose.Capabilities.Capability
   is
   begin
      if Index > Natural (Item.Caps.Length) then
         raise No_Such_Cap with
           "no cap" & Positive'Image (Index) & " in a set of"
           & Natural'Image (Natural (Item.Caps.Length));
      end if;
      return Item.Caps (Index);
   end Get_Cap;

   -----------
   -- Image --
   -----------

   function Image (X : Natural) return String is
      Text : constant String := Natural'Image (X);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   ------------------
   -- Open_Console --
   ------------------

   procedure Open_Console is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Console_Writer_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         Rose.Standard_Streams.Set (Standard_Output => Params.Caps (0));
      end if;
   end Open_Console;

   -------------
   -- Release --
   -------------

   --  The generated handler then detaches the set, which rescinds every
   --  cap to it.

   overriding procedure Release (Item : in out Set_Object) is
   begin
      for Cap of Item.Caps loop
         if Cap /= Rose.Capabilities.Null_Capability then
            Rose.System_Calls.Delete_Cap (Cap);
         end if;
      end loop;
      Used := Used - Natural (Item.Caps.Length) - Set_Overhead;
      Set_Count := Set_Count - 1;
      Item.Caps.Clear;
      Item.Active := False;
   end Release;

   --------------
   -- Restored --
   --------------

   --  The sets are in this process's memory, and their caps in its table,
   --  so a restore brings them back as they were. The boot test compares
   --  this line with what the install left (#825).

   overriding procedure Restored (Item : in out Constructor_Object) is
      pragma Unreferenced (Item);
   begin
      if Rose.Standard_Streams.Standard_Output
        /= Rose.Capabilities.Null_Capability
      then
         Ada.Text_IO.Put_Line
           ("caps: restored sets " & Image (Set_Count)
            & "; caps " & Image (Used - Set_Count * Set_Overhead));
      end if;
   end Restored;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Open_Console;
      Rose.Interfaces.Constructor.Server.Publish
        (Context, The_Constructor'Access);
      Rose.Interfaces.Restorable.Server.Attach
        (Context, The_Constructor'Access);
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Caps.Server;
