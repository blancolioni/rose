with System.Storage_Elements;

with Ada.Containers.Vectors;

with Rose.Invocation;
with Rose.Objects;

with Rose.Server;
with Rose.Trace;
with Rose.System_Calls;

with Rose.Interfaces.Caps.Cap_Set.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Processes.Exec.Server;
with Rose.Interfaces.Processes.Launch.Client;
with Rose.Interfaces.Processes.Launch.Server;
with Rose.Interfaces.Processes.Process_Control.Client;
with Rose.Interfaces.Processes.Restorable.Server;
with Rose.Interfaces.Processes.Starting_Process.Client;
with Rose.Interfaces.Storage.Bank;
with Rose.Interfaces.Streams.Reader;
with Rose.Interfaces.Streams.Writer;

with Exec.Library;
with Exec.Output;

package body Exec.Server is

   subtype Launch_Class is Rose.Interfaces.Processes.Launch
     .Launch_Object'Class;
   subtype Control_Class is Rose.Interfaces.Processes.Process_Control
     .Process_Control_Object'Class;

   --  One object serves Exec, and takes the restore notice. Each
   --  installed program is an object of its own, which serves its Launch
   --  interface; Exec's own Launch operations launch nothing.

   type Exec_Object is
     new Rose.Interfaces.Processes.Exec.Server.Exec_Interface
     and Rose.Interfaces.Processes.Restorable.Server.Restorable_Interface
     with null record;

   overriding function Exec_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability;

   overriding function Restorable_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability;

   overriding procedure Restored (Item : in out Exec_Object);
   --  Register the processes that exec launched with their banks again
   --  (#875).

   Bank_Signal : constant Rose.Server.Signal_Bit := 8;
   --  The bit of the signal that the store raises when the bank of a
   --  process that exec launched is destroyed (ADR 0006, ADR 0014).

   procedure On_Bank_Signal;
   --  A process that exec launched has ended, or one of them has.

   overriding function Launch_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability;

   overriding function Install
     (Item         : in out Exec_Object;
      Executable   : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name         : String;
      Install_Caps : Rose.Capabilities.Capability)
      return Launch_Class;

   overriding procedure Set_Standard_Output
     (Item   : in out Exec_Object;
      Output : in out Rose.Interfaces.Streams.Writer.Writer_Object'Class);

   overriding function Update
     (Item         : in out Exec_Object;
      Executable   : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name         : String;
      Install_Caps : Rose.Capabilities.Capability)
      return Launch_Class;

   overriding function Launch
     (Item : in out Exec_Object;
      Bank : Rose.Capabilities.Capability)
      return Control_Class;

   overriding function Launch_Command
     (Item : in out Exec_Object;
      Bank : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   overriding function Metadata (Item : in out Exec_Object) return String;

   type Program_Object is
     new Rose.Interfaces.Processes.Launch.Server.Launch_Interface with
      record
         Id : Rose.Objects.Capability_Identifier := 0;
         --  The program's number in Exec.Library.
      end record;

   overriding function Launch_Cap
     (Item : in out Program_Object)
      return Rose.Capabilities.Capability;

   overriding function Launch
     (Item : in out Program_Object;
      Bank : Rose.Capabilities.Capability)
      return Control_Class;

   overriding function Launch_Command
     (Item : in out Program_Object;
      Bank : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   overriding function Metadata (Item : in out Program_Object) return String;

   type Program_Access is access Program_Object;

   package Program_Vectors is
     new Ada.Containers.Vectors (Positive, Program_Access);

   Programs : Program_Vectors.Vector;
   --  The object of each program, by its number in Exec.Library, so
   --  that Update can give the installed version's Launch again.

   function Launch_Of
     (Id : Rose.Objects.Capability_Identifier)
      return Launch_Class;
   --  The Launch interface of program Id, whose object is made the first
   --  time; or a null cap if Id is 0.

   function Newest
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Capability_Identifier;
   --  The newest version of program Id, which a launch through Id's caps
   --  launches (#902). Fails the call if there is no program Id.

   procedure Send_Launch_Arguments
     (Params : in out Rose.Invocation.Invocation_Record;
      Id     : Rose.Objects.Capability_Identifier;
      Bank   : Rose.Capabilities.Capability);
   --  Send what the ELF server's Launch and Launch_Command take: the name
   --  of program Id, which becomes the process's name (#1005), its
   --  image, the bank to launch it against: Bank, or exec's own if Bank
   --  is null (ADR 0015, #750), and whether it is transient (#524).

   function Is_Transient
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean;
   --  True if program Id's command metadata has the line "transient
   --  true", which idle writes for a command with the Transient aspect
   --  (idle#127): its processes are never saved in a checkpoint.

   function Start_Launched
     (Params : Rose.Invocation.Invocation_Record;
      Id     : Rose.Objects.Capability_Identifier;
      Caps   : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;
   --  Params is the reply to the ELF server's Launch or Launch_Command,
   --  which has made a process of program Id but not started it (#234):
   --  give it Caps, one at a time, start it, and return what Start
   --  gives, or null if the launch failed. That is kept until the next
   --  launch, because the reply to the launch sends it, and then
   --  deleted. A process that starts is added to Exec.Library's, with
   --  its bank (#875).

   Max_Install_Caps : constant := 16;

   function Read_Cap_Set
     (Set_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability_Array;
   --  The caps in the cap set Set_Cap, at most Max_Install_Caps, or none
   --  if it is null; and delete exec's cap to the set.

   Last_Result : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   Standard_Output : Rose.Capabilities.Capability := Console_Cap;
   --  Given to every program exec launches.  It starts as the boot
   --  console, and init replaces it once the persistent console runs.

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability);

   procedure Not_A_Program;
   --  Fail a launch through Exec itself, which is not a program.

   Context  : Rose.Server.Server_Context;
   The_Exec : aliased Exec_Object;

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin

      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);

      Exec.Output.Set (Standard_Output);
      Exec.Library.Set_Region (Region_Cap);
      Exec.Library.Set_Signal
        (Rose.System_Calls.Create_Signal_Cap (Bank_Signal));
      Rose.Server.Set_Signal_Handler
        (Context, Bank_Signal, On_Bank_Signal'Access);
      Rose.Interfaces.Processes.Exec.Server.Publish (Context, The_Exec'Access);
      Rose.Interfaces.Processes.Restorable.Server.Attach
        (Context, The_Exec'Access);
   end Create_Server;

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability) is
   begin
      Rose.System_Calls.Delete_Cap (Cap);
   end Delete_Cap;

   --------------
   -- Exec_Cap --
   --------------

   overriding function Exec_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Exec.Server.Cap_Of (Context, Item));

   -------------
   -- Install --
   -------------

   overriding function Install
     (Item         : in out Exec_Object;
      Executable   : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name         : String;
      Install_Caps : Rose.Capabilities.Capability)
      return Launch_Class
   is
      pragma Unreferenced (Item);
      Id : constant Rose.Objects.Capability_Identifier :=
        Exec.Library.Install
          (Executable, Name, Read_Cap_Set (Install_Caps));
   begin
      return Launch_Of (Id);
   end Install;

   ------------------
   -- Is_Transient --
   ------------------

   function Is_Transient
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean
   is
      Key  : constant String := "transient true";
      Text : String (1 .. 4096);
      Last : Natural;
      Line : Positive := 1;
   begin
      Exec.Library.Get_Metadata (Id, Text, Last);

      --  The metadata is lines of a key and a value, separated by line
      --  feeds.

      for Index in 1 .. Last + 1 loop
         if Index > Last or else Text (Index) = Character'Val (10) then
            if Text (Line .. Index - 1) = Key then
               return True;
            end if;
            Line := Index + 1;
         end if;
      end loop;
      return False;
   end Is_Transient;

   ------------
   -- Launch --
   ------------

   overriding function Launch
     (Item : in out Exec_Object;
      Bank : Rose.Capabilities.Capability)
      return Control_Class
   is
      pragma Unreferenced (Item, Bank);
   begin
      Not_A_Program;
      return Rose.Interfaces.Processes.Process_Control.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Launch;

   ------------
   -- Launch --
   ------------

   overriding function Launch
     (Item : in out Program_Object;
      Bank : Rose.Capabilities.Capability)
      return Control_Class
   is
      use Rose.System_Calls;
      use type Rose.Capabilities.Capability_Array;
      Id     : constant Rose.Objects.Capability_Identifier :=
        Newest (Item.Id);
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Create_Process_Cap);
      Send_Launch_Arguments (Params, Id, Bank);
      Receive_Caps (Params, 1);
      Invoke_Capability (Params);
      Delete_Cap (Bank);

      return Rose.Interfaces.Processes.Process_Control.Client
        .From_Cap
          (Start_Launched
             (Params, Id,
              Exec.Library.Install_Caps (Id) & Standard_Output));
   end Launch;

   ----------------
   -- Launch_Cap --
   ----------------

   overriding function Launch_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Exec.Server.Cap_Of
         (Context, Item, Rose.Interfaces.Processes.Exec.Launch_Interface));

   ----------------
   -- Launch_Cap --
   ----------------

   overriding function Launch_Cap
     (Item : in out Program_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Launch.Server.Cap_Of (Context, Item));

   --------------------
   -- Launch_Command --
   --------------------

   overriding function Launch_Command
     (Item : in out Exec_Object;
      Bank : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Item, Bank);
   begin
      Not_A_Program;
      return Rose.Capabilities.Null_Capability;
   end Launch_Command;

   --------------------
   -- Launch_Command --
   --------------------

   overriding function Launch_Command
     (Item : in out Program_Object;
      Bank : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      use Rose.System_Calls;
      Id     : constant Rose.Objects.Capability_Identifier :=
        Newest (Item.Id);
      Params : aliased Rose.Invocation.Invocation_Record;
   begin

      --  A command has only the caps it is given (ADR 0002): no install
      --  caps, and no console.

      Rose.Trace.Event ("launch command, program ", Natural (Id));
      Initialize_Send (Params, Launch_Command_Cap);
      Send_Launch_Arguments (Params, Id, Bank);
      Receive_Caps (Params, 1);
      Invoke_Capability (Params);
      Rose.Trace.Event ("command launched");
      Delete_Cap (Bank);

      return Start_Launched (Params, Id, (1 .. 0 => 0));
   end Launch_Command;

   ---------------
   -- Launch_Of --
   ---------------

   function Launch_Of
     (Id : Rose.Objects.Capability_Identifier)
      return Launch_Class
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      if Id = 0 then
         return Rose.Interfaces.Processes.Launch.Client.From_Cap
           (Rose.Capabilities.Null_Capability);
      end if;

      while Programs.Last_Index < Positive (Id) loop
         Programs.Append (null);
      end loop;

      if Programs (Positive (Id)) = null then
         declare
            Program : constant Program_Access :=
                        new Program_Object'(Id => Id);
         begin
            Rose.Interfaces.Processes.Launch.Server.Attach (Context, Program);
            Programs (Positive (Id)) := Program;
         end;
      end if;

      return Rose.Interfaces.Processes.Launch.Client.From_Cap
        (Rose.Interfaces.Processes.Launch.Server.Cap_Of
           (Context, Programs (Positive (Id)).all));
   end Launch_Of;

   --------------
   -- Metadata --
   --------------

   overriding function Metadata (Item : in out Exec_Object) return String is
      pragma Unreferenced (Item);
   begin
      Not_A_Program;
      return "";
   end Metadata;

   --------------
   -- Metadata --
   --------------

   overriding function Metadata
     (Item : in out Program_Object)
      return String
   is
      Text : String (1 .. 4096);
      Last : Natural;
   begin
      Exec.Library.Get_Metadata (Newest (Item.Id), Text, Last);
      return Text (1 .. Last);
   end Metadata;

   ------------
   -- Newest --
   ------------

   function Newest
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Capability_Identifier
   is
      use type Rose.Objects.Capability_Identifier;
      Result : constant Rose.Objects.Capability_Identifier :=
        Exec.Library.Newest (Id);
   begin
      if Result = 0 then
         raise Constraint_Error with "exec: no such program";
      end if;
      return Result;
   end Newest;

   -------------------
   -- Not_A_Program --
   -------------------

   procedure Not_A_Program is
   begin
      raise Constraint_Error with "exec: Exec is not a program";
   end Not_A_Program;

   --------------------
   -- On_Bank_Signal --
   --------------------

   procedure On_Bank_Signal is
   begin
      Exec.Library.Check_Processes;
   end On_Bank_Signal;

   ------------------
   -- Read_Cap_Set --
   ------------------

   function Read_Cap_Set
     (Set_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability_Array
   is
      use type Rose.Capabilities.Capability;
      Set : Rose.Interfaces.Caps.Cap_Set.Client.Cap_Set_Client;
   begin
      if Set_Cap = Rose.Capabilities.Null_Capability then
         return (1 .. 0 => 0);
      end if;

      Rose.Interfaces.Caps.Cap_Set.Client.Open (Set, Set_Cap);

      declare
         Caps : Rose.Capabilities.Capability_Array
           (1 .. Natural'Min (Set.Length, Max_Install_Caps));
      begin
         for I in Caps'Range loop
            Caps (I) := Set.Get_Cap (I);
         end loop;
         Rose.Interfaces.Caps.Cap_Set.Client.Close (Set);
         Delete_Cap (Set_Cap);
         return Caps;
      end;
   end Read_Cap_Set;

   --------------------
   -- Restorable_Cap --
   --------------------

   overriding function Restorable_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Restorable.Server.Cap_Of (Context, Item));

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Exec_Object) is
      pragma Unreferenced (Item);
   begin
      Exec.Library.Register_Again;
   end Restored;

   ---------------------------
   -- Send_Launch_Arguments --
   ---------------------------

   procedure Send_Launch_Arguments
     (Params : in out Rose.Invocation.Invocation_Record;
      Id     : Rose.Objects.Capability_Identifier;
      Bank   : Rose.Capabilities.Capability)
   is
      use type Rose.Capabilities.Capability;
      Transient : constant Boolean := Is_Transient (Id);
      --  Read first: reading the metadata uses the invocation buffer,
      --  which Send_Text fills with the name.
      Name : String (1 .. Exec.Library.Max_Name_Length);
      Last : Natural;
   begin
      Exec.Library.Get_Program_Name (Id, Name, Last);
      Rose.System_Calls.Send_Text (Params, Name (1 .. Last));
      Rose.System_Calls.Send_Cap (Params, Exec.Library.Image_Region (Id));
      Rose.System_Calls.Send_Cap
        (Params,
         (if Bank /= Rose.Capabilities.Null_Capability
          then Bank else Bank_Cap));
      Rose.System_Calls.Send_Word (Params, Natural (Boolean'Pos (Transient)));
   end Send_Launch_Arguments;

   -------------------------
   -- Set_Standard_Output --
   -------------------------

   overriding procedure Set_Standard_Output
     (Item   : in out Exec_Object;
      Output : in out Rose.Interfaces.Streams.Writer.Writer_Object'Class)
   is
      pragma Unreferenced (Item);
   begin
      Standard_Output := Output.Writer_Cap;
      Exec.Output.Set (Standard_Output);
   end Set_Standard_Output;

   --------------------
   -- Start_Launched --
   --------------------

   function Start_Launched
     (Params : Rose.Invocation.Invocation_Record;
      Id     : Rose.Objects.Capability_Identifier;
      Caps   : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      Starting_Cap : Rose.Capabilities.Capability;
      Starting     : Rose.Interfaces.Processes.Starting_Process.Client
        .Starting_Process_Client;
      Bank         : Rose.Capabilities.Capability :=
                       Rose.Capabilities.Null_Capability;
      --  The process's own bank (#875).
   begin
      if Last_Result /= Rose.Capabilities.Null_Capability then
         Delete_Cap (Last_Result);
         Last_Result := Rose.Capabilities.Null_Capability;
      end if;

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
        or else Params.Caps (0) = Rose.Capabilities.Null_Capability
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      Starting_Cap := Params.Caps (0);
      Rose.Interfaces.Processes.Starting_Process.Client.Open
        (Starting, Starting_Cap);

      for Cap of Caps loop
         Starting.Add_Cap (Cap);
      end loop;

      --  The bank is asked for before Start, while elf still has the
      --  process. Without it, the process is never followed, and its
      --  program's image is kept.

      begin
         declare
            Process_Bank : Rose.Interfaces.Storage.Bank.Bank_Object'Class :=
              Starting.Bank;
         begin
            Bank := Process_Bank.Bank_Cap;
         end;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            Bank := Rose.Capabilities.Null_Capability;
      end;

      --  It is registered before it starts, so that it can't end first.
      --  If the start fails, elf ends the process, which destroys its
      --  bank, and the registration goes with it.

      Exec.Library.Add_Process (Id, Bank);

      Last_Result := Starting.Start;
      Rose.Interfaces.Processes.Starting_Process.Client.Close (Starting);
      Delete_Cap (Starting_Cap);
      return Last_Result;
   end Start_Launched;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Item         : in out Exec_Object;
      Executable   : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name         : String;
      Install_Caps : Rose.Capabilities.Capability)
      return Launch_Class
   is
      pragma Unreferenced (Item);
      Changed : Boolean;
      Id      : constant Rose.Objects.Capability_Identifier :=
        Exec.Library.Update
          (Executable, Name, Read_Cap_Set (Install_Caps), Changed);
   begin
      if Changed then
         Exec.Output.Put_Line ("exec: new version of " & Name);
      end if;
      return Launch_Of (Id);
   end Update;

end Exec.Server;
