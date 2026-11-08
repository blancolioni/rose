with System.Storage_Elements;

with Rose.Invocation;
with Rose.Objects;

with Rose.Server;
with Rose.Trace;
with Rose.System_Calls;

with Rose.Interfaces.Exec.Server;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Launch.Server;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Reader;
with Rose.Interfaces.Writer;

with Exec.Library;
with Exec.Output;

package body Exec.Server is

   subtype Launch_Class is Rose.Interfaces.Launch.Launch_Object'Class;
   subtype Process_Class is Rose.Interfaces.Process.Process_Object'Class;

   --  One object serves Exec. Each installed program is an object of its
   --  own, which serves its Launch interface; Exec's own Launch
   --  operations launch nothing.

   type Exec_Object is
     new Rose.Interfaces.Exec.Server.Exec_Interface with null record;

   overriding function Exec_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability;

   overriding function Launch_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability;

   overriding function Install
     (Item         : in out Exec_Object;
      Executable   : in out Rose.Interfaces.Reader.Reader_Object'Class;
      Name         : String;
      Install_Caps : Rose.Capabilities.Capability_Array)
      return Launch_Class;

   overriding procedure Set_Standard_Output
     (Item   : in out Exec_Object;
      Output : in out Rose.Interfaces.Writer.Writer_Object'Class);

   overriding function Launch
     (Item : in out Exec_Object;
      Caps : Rose.Capabilities.Capability_Array)
      return Process_Class;

   overriding function Launch_Command
     (Item : in out Exec_Object;
      Caps : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;

   overriding function Metadata (Item : in out Exec_Object) return String;

   type Program_Object is
     new Rose.Interfaces.Launch.Server.Launch_Interface with
      record
         Id : Rose.Objects.Capability_Identifier := 0;
         --  The program's number in Exec.Library.
      end record;

   overriding function Launch_Cap
     (Item : in out Program_Object)
      return Rose.Capabilities.Capability;

   overriding function Launch
     (Item : in out Program_Object;
      Caps : Rose.Capabilities.Capability_Array)
      return Process_Class;

   overriding function Launch_Command
     (Item : in out Program_Object;
      Caps : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;

   overriding function Metadata (Item : in out Program_Object) return String;

   type Program_Access is access Program_Object;

   procedure Send_Image
     (Params : in out Rose.Invocation.Invocation_Record;
      Id     : Rose.Objects.Capability_Identifier);
   --  Send the image and storage of program Id, as the ELF server's
   --  Launch and Launch_Command take them.

   function Result
     (Params : Rose.Invocation.Invocation_Record)
      return Rose.Capabilities.Capability;
   --  The cap in the reply Params to a launch, or null if it failed. It
   --  is kept until the next launch, because the reply to the launch
   --  sends it, and then deleted.

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
      Rose.Interfaces.Exec.Server.Publish (Context, The_Exec'Access);
   end Create_Server;

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send
        (Params, Rose.System_Calls.Delete_Cap_Capability);
      Rose.System_Calls.Send_Cap (Params, Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Delete_Cap;

   --------------
   -- Exec_Cap --
   --------------

   overriding function Exec_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Exec.Server.Cap_Of (Item));

   -------------
   -- Install --
   -------------

   overriding function Install
     (Item         : in out Exec_Object;
      Executable   : in out Rose.Interfaces.Reader.Reader_Object'Class;
      Name         : String;
      Install_Caps : Rose.Capabilities.Capability_Array)
      return Launch_Class
   is
      pragma Unreferenced (Item);
      use type Rose.Objects.Capability_Identifier;
      Id : constant Rose.Objects.Capability_Identifier :=
        Exec.Library.Install (Executable, Name, Install_Caps);
   begin
      if Id = 0 then
         return Rose.Interfaces.Launch.Client.From_Cap
           (Rose.Capabilities.Null_Capability);
      end if;

      declare
         Program : constant Program_Access := new Program_Object'(Id => Id);
      begin
         Rose.Interfaces.Launch.Server.Attach (Context, Program);
         return Rose.Interfaces.Launch.Client.From_Cap
           (Rose.Interfaces.Launch.Server.Cap_Of (Program.all));
      end;
   end Install;

   ------------
   -- Launch --
   ------------

   overriding function Launch
     (Item : in out Exec_Object;
      Caps : Rose.Capabilities.Capability_Array)
      return Process_Class
   is
      pragma Unreferenced (Item, Caps);
   begin
      Not_A_Program;
      return Rose.Interfaces.Process.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Launch;

   ------------
   -- Launch --
   ------------

   overriding function Launch
     (Item : in out Program_Object;
      Caps : Rose.Capabilities.Capability_Array)
      return Process_Class
   is
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Create_Process_Cap);
      Send_Image (Params, Item.Id);
      Exec.Library.Send_Install_Caps (Item.Id, Params);

      for Cap of Caps loop
         Send_Cap (Params, Cap);
      end loop;

      Send_Cap (Params, Standard_Output);

      Invoke_Capability (Params);

      for Cap of Caps loop
         Delete_Cap (Cap);
      end loop;

      return Rose.Interfaces.Process.Client.From_Cap (Result (Params));
   end Launch;

   ----------------
   -- Launch_Cap --
   ----------------

   overriding function Launch_Cap
     (Item : in out Exec_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Exec.Server.Cap_Of
         (Item, Rose.Interfaces.Exec.Launch_Interface));

   ----------------
   -- Launch_Cap --
   ----------------

   overriding function Launch_Cap
     (Item : in out Program_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Launch.Server.Cap_Of (Item));

   --------------------
   -- Launch_Command --
   --------------------

   overriding function Launch_Command
     (Item : in out Exec_Object;
      Caps : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Item, Caps);
   begin
      Not_A_Program;
      return Rose.Capabilities.Null_Capability;
   end Launch_Command;

   --------------------
   -- Launch_Command --
   --------------------

   overriding function Launch_Command
     (Item : in out Program_Object;
      Caps : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
      Name   : String (1 .. 16);
      Last   : Natural;
   begin
      --  A command has only the caps it is given (ADR 0002): no install
      --  caps, and no console.

      Rose.Trace.Event ("launch command, program ", Natural (Item.Id));
      Initialize_Send (Params, Launch_Command_Cap);
      Send_Image (Params, Item.Id);

      for Cap of Caps loop
         Send_Cap (Params, Cap);
      end loop;

      Exec.Library.Get_Program_Name (Item.Id, Name, Last);
      Send_Text (Params, Name (1 .. Last));
      Receive_Caps (Params, 1);
      Invoke_Capability (Params);
      Rose.Trace.Event ("command launched");

      for Cap of Caps loop
         Delete_Cap (Cap);
      end loop;

      return Result (Params);
   end Launch_Command;

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
      Exec.Library.Get_Metadata (Item.Id, Text, Last);
      return Text (1 .. Last);
   end Metadata;

   -------------------
   -- Not_A_Program --
   -------------------

   procedure Not_A_Program is
   begin
      raise Constraint_Error with "exec: Exec is not a program";
   end Not_A_Program;

   ------------
   -- Result --
   ------------

   function Result
     (Params : Rose.Invocation.Invocation_Record)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
   begin
      if Last_Result /= Rose.Capabilities.Null_Capability then
         Delete_Cap (Last_Result);
      end if;

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         Last_Result := Rose.Capabilities.Null_Capability;
      else
         Last_Result := Params.Caps (0);
      end if;
      return Last_Result;
   end Result;

   ----------------
   -- Send_Image --
   ----------------

   procedure Send_Image
     (Params : in out Rose.Invocation.Invocation_Record;
      Id     : Rose.Objects.Capability_Identifier)
   is
   begin
      Rose.System_Calls.Send_Cap (Params, Exec.Library.Image_Region (Id));
      Rose.System_Calls.Send_Cap (Params, Storage_Cap);
   end Send_Image;

   -------------------------
   -- Set_Standard_Output --
   -------------------------

   overriding procedure Set_Standard_Output
     (Item   : in out Exec_Object;
      Output : in out Rose.Interfaces.Writer.Writer_Object'Class)
   is
      pragma Unreferenced (Item);
   begin
      Standard_Output := Output.Writer_Cap;
      Exec.Output.Set (Standard_Output);
   end Set_Standard_Output;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Exec.Server;
