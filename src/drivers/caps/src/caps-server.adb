with Ada.Containers.Vectors;
with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Server;
with Rose.Standard_Streams;
with Rose.System_Calls;

with Rose.Interfaces.Caps.Bank_Constructor.Server;
with Rose.Interfaces.Caps.Releasable_Cap_Set.Server;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Processes.Restorable.Server;
with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Storage.Dependent.Client;

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

   Bank_Overhead : constant := 10;
   --  The caps that the server holds as well for a set made in a bank
   --  (#849): the bank's interface cap and its six operations, and the
   --  Dependent's interface cap and its two operations.

   Limit        : constant := Table_Size - Reserve;

   Used         : Natural := 0;
   --  The caps held for sets: their contents, Set_Overhead each, and
   --  Bank_Overhead for each set made in a bank.

   Set_Count    : Natural := 0;

   Bank_Signal  : constant Rose.Server.Signal_Bit := 8;
   --  The bit of the signal that the storage server raises when a bank
   --  that a set was made in is destroyed (ADR 0006, ADR 0014).

   Signal_Cap   : Rose.Capabilities.Capability :=
                    Rose.Capabilities.Null_Capability;
   --  A signal cap for Bank_Signal, which each set made in a bank is
   --  registered with.

   Cap_Set_Full : exception;
   No_Such_Cap  : exception;
   Bank_Gone    : exception;

   package Cap_Vectors is
     new Ada.Containers.Vectors (Positive, Rose.Capabilities.Capability,
                                 Rose.Capabilities."=");

   Context : Rose.Server.Server_Context;

   type Set_Object;
   type Set_Access is access all Set_Object;

   type Set_Object is limited
     new Rose.Interfaces.Caps.Releasable_Cap_Set.Server
       .Releasable_Cap_Set_Interface with
      record
         Active    : Boolean := False;
         Caps      : Cap_Vectors.Vector;
         Next      : Set_Access;
         In_Bank   : Boolean := False;
         --  Made with Create_In, so it is released when its bank is
         --  destroyed.
         Bank      : Rose.Interfaces.Storage.Bank.Client.Bank_Client;
         --  The bank, kept to register the set with it again after a
         --  restore, since registrations aren't saved.
         Dependent : Rose.Interfaces.Storage.Dependent.Client
           .Dependent_Client;
         --  The set's registration with the bank.
      end record;

   overriding function Releasable_Cap_Set_Cap
     (Item : in out Set_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Caps.Releasable_Cap_Set.Server.Cap_Of (Context, Item));

   overriding function Cap_Set_Cap
     (Item : in out Set_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Caps.Releasable_Cap_Set.Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Caps.Releasable_Cap_Set.Cap_Set_Interface));

   overriding function Resource_Cap
     (Item : in out Set_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Caps.Releasable_Cap_Set.Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Caps.Releasable_Cap_Set.Resource_Interface));

   overriding procedure Append
     (Item : in out Set_Object;
      Cap  : Rose.Capabilities.Capability);

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
     new Rose.Interfaces.Caps.Bank_Constructor.Server
       .Bank_Constructor_Interface
     and Rose.Interfaces.Processes.Restorable.Server.Restorable_Interface
     with null record;

   overriding function Bank_Constructor_Cap
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Caps.Bank_Constructor.Server.Cap_Of (Context, Item));

   overriding function Constructor_Cap
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Caps.Bank_Constructor.Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Caps.Bank_Constructor.Constructor_Interface));

   overriding function Restorable_Cap
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Restorable.Server.Cap_Of (Context, Item));

   overriding function Create
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability;

   overriding function Create_In
     (Item : in out Constructor_Object;
      Bank : in out Rose.Interfaces.Storage.Bank.Bank_Object'Class)
      return Rose.Capabilities.Capability;

   overriding procedure Restored (Item : in out Constructor_Object);

   The_Constructor : aliased Constructor_Object;

   Sets            : Set_Access;
   --  Every set that has been made. A released set is kept, inactive,
   --  for Create to use again.

   function Bank_Destroyed (Set : not null Set_Access) return Boolean;
   --  True if the bank that Set was made in has been destroyed, or the
   --  storage server no longer knows the set's registration.

   function Image (X : Natural) return String;
   --  X, with no leading space.

   function New_Set return Set_Access;
   --  An empty set, attached, and counted.

   procedure On_Bank_Signal;
   --  A bank that a set was made in has been destroyed: release each set
   --  whose bank is gone, as if its holder had.

   procedure Open_Console;
   --  Write to the console's Writer, if the installer gave one.

   function Register (Set : not null Set_Access) return Boolean;
   --  Register Set with its bank. False if the bank refuses, because it
   --  has been destroyed, or the storage server has no room.

   procedure Remove (Set : Set_Access);
   --  Release Set, and detach it, so that every cap to it fails.

   ------------
   -- Append --
   ------------

   --  When the sets are full, Append raises. The cap has arrived in this
   --  process's table already, and the generated handler deletes it when
   --  the operation raises (idle#95), so it must not be kept or deleted
   --  here.

   overriding procedure Append
     (Item : in out Set_Object;
      Cap  : Rose.Capabilities.Capability)
   is
   begin
      if Used = Limit then
         raise Cap_Set_Full with
           "the cap sets are full: " & Image (Used) & " caps are held";
      end if;

      Item.Caps.Append (Cap);
      Used := Used + 1;
   end Append;

   --------------------
   -- Bank_Destroyed --
   --------------------

   function Bank_Destroyed (Set : not null Set_Access) return Boolean is
   begin
      return Set.Dependent.Destroyed;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return True;
   end Bank_Destroyed;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Item : in out Constructor_Object)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Item);
   begin
      if Set_Overhead > Limit - Used then
         raise Cap_Set_Full with
           "no room for another cap set: " & Image (Used)
           & " caps are held";
      end if;

      return Rose.Interfaces.Caps.Releasable_Cap_Set.Server
        .Interface_Cap (Context, New_Set);
   end Create;

   ---------------
   -- Create_In --
   ---------------

   --  The generated handler leaves the bank's cap to this server when
   --  Create_In returns, and deletes it when it raises. When the bank
   --  refuses the set, Remove has deleted it already, by closing the
   --  set's Bank client, and nothing makes a cap before the handler
   --  deletes it again, so that finds an empty slot.

   overriding function Create_In
     (Item : in out Constructor_Object;
      Bank : in out Rose.Interfaces.Storage.Bank.Bank_Object'Class)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Item);
   begin
      if Set_Overhead + Bank_Overhead > Limit - Used then
         raise Cap_Set_Full with
           "no room for another cap set: " & Image (Used)
           & " caps are held";
      end if;

      declare
         Set : constant Set_Access := New_Set;
      begin
         Set.Bank.Open (Bank.Bank_Cap);
         Set.In_Bank := True;
         Used := Used + Bank_Overhead;

         if not Register (Set) then
            Remove (Set);
            raise Bank_Gone with "the bank takes no more sets";
         end if;

         return Rose.Interfaces.Caps.Releasable_Cap_Set.Server
           .Interface_Cap (Context, Set);
      end;
   end Create_In;

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

   -------------
   -- New_Set --
   -------------

   function New_Set return Set_Access is
      Set : Set_Access := Sets;
   begin
      while Set /= null and then Set.Active loop
         Set := Set.Next;
      end loop;

      if Set = null then
         Set := new Set_Object;
         Set.Next := Sets;
         Sets := Set;
      end if;

      Set.Active := True;
      Set.In_Bank := False;
      Set.Caps.Clear;
      Rose.Interfaces.Caps.Releasable_Cap_Set.Server.Attach (Context, Set);
      Used := Used + Set_Overhead;
      Set_Count := Set_Count + 1;
      return Set;
   end New_Set;

   --------------------
   -- On_Bank_Signal --
   --------------------

   procedure On_Bank_Signal is
      Set : Set_Access := Sets;
   begin
      while Set /= null loop
         if Set.Active and then Set.In_Bank
           and then Bank_Destroyed (Set)
         then
            Remove (Set);
         end if;
         Set := Set.Next;
      end loop;
   end On_Bank_Signal;

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

   --------------
   -- Register --
   --------------

   function Register (Set : not null Set_Access) return Boolean is
   begin
      declare
         Dependent : Rose.Interfaces.Storage.Dependent.Dependent_Object'Class
           := Set.Bank.Add_Dependent (Signal_Cap);
      begin
         if Dependent.Dependent_Cap = Rose.Capabilities.Null_Capability then
            return False;
         end if;
         Set.Dependent.Open (Dependent.Dependent_Cap);
      end;
      return True;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return False;
   end Register;

   -------------
   -- Release --
   -------------

   --  The generated handler then detaches the set, which rescinds every
   --  cap to it. A set made in a bank also ends its registration with the
   --  bank, which may have been destroyed already.

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

      if Item.In_Bank then
         begin
            Item.Dependent.Release;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;
         Item.Dependent.Forget;
         Item.Bank.Close;
         Used := Used - Bank_Overhead;
         Item.In_Bank := False;
      end if;
   end Release;

   ------------
   -- Remove --
   ------------

   procedure Remove (Set : Set_Access) is
   begin
      Set.Release;
      Rose.Interfaces.Caps.Releasable_Cap_Set.Server.Detach (Context, Set);
   end Remove;

   --------------
   -- Restored --
   --------------

   --  The sets are in this process's memory, and their caps in its table,
   --  so a restore brings them back as they were. Their registrations
   --  with their banks aren't saved, so each set made in a bank is
   --  registered again, and released if its bank is gone. The boot test
   --  compares the line it writes with what the install left (#825).

   overriding procedure Restored (Item : in out Constructor_Object) is
      pragma Unreferenced (Item);
      Set  : Set_Access := Sets;
      Held : Natural := 0;
   begin
      while Set /= null loop
         if Set.Active and then Set.In_Bank then
            Set.Dependent.Forget;
            if not Register (Set) then
               Remove (Set);
            end if;
         end if;
         if Set.Active then
            Held := Held + Natural (Set.Caps.Length);
         end if;
         Set := Set.Next;
      end loop;

      if Rose.Standard_Streams.Standard_Output
        /= Rose.Capabilities.Null_Capability
      then
         Ada.Text_IO.Put_Line
           ("caps: restored sets " & Image (Set_Count)
            & "; caps " & Image (Held));
      end if;
   end Restored;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Open_Console;
      Signal_Cap := Rose.System_Calls.Create_Signal_Cap (Bank_Signal);
      Rose.Server.Set_Signal_Handler
        (Context, Bank_Signal, On_Bank_Signal'Access);
      Rose.Interfaces.Caps.Bank_Constructor.Server.Publish
        (Context, The_Constructor'Access);
      Rose.Interfaces.Processes.Restorable.Server.Attach
        (Context, The_Constructor'Access);
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Caps.Server;
