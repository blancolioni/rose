with System.Storage_Elements;

with Rose.Invocation;
with Rose.Objects;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Exec.Server;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Stream_Reader.Client;

with Exec.Library;

package body Exec.Server is

   function On_Install
     (Id        : Rose.Objects.Capability_Identifier;
      ELF_Image : Rose.Capabilities.Capability;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;

   function On_Launch
     (Id          : Rose.Objects.Capability_Identifier;
      Caps        : Rose.Capabilities.Capability_Array;
      Environment : System.Storage_Elements.Storage_Array)
      return Rose.Capabilities.Capability;

   function On_Launch_Command
     (Id   : Rose.Objects.Capability_Identifier;
      Caps : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;

   procedure On_Metadata
     (Id     : Rose.Objects.Capability_Identifier;
      Result : out String;
      Last   : out Natural);

   procedure On_Get_Launch_Interface
     (Parameters : in out Rose.Invocation.Invocation_Record);
   --  Reply with the caps of the Launch interface of the installed
   --  program whose identifier the invoked cap has.

   procedure Send_Image
     (Params : in out Rose.Invocation.Invocation_Record;
      Id     : Rose.Objects.Capability_Identifier);
   --  Send the image and storage of program Id, as the ELF server's
   --  Launch and Launch_Command take them.

   Context : Rose.Server.Server_Context;
   Region : Rose.Interfaces.Region.Client.Region_Client;

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin

      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);

      Rose.Interfaces.Region.Client.Open (Region, Region_Cap);
      Exec.Library.Set_Region (Region);

      --  Installed programs have their own Launch interfaces. This handler
      --  is registered first, so it is used instead of the generated one,
      --  which has no instances.

      Rose.Server.Register_Handler
        (Context,
         Rose.Interfaces.Launch.Launch_Interface,
         On_Get_Launch_Interface'Access);

      Rose.Interfaces.Exec.Server.Publish_Interface
        (Server_Context => Context,
         Install        => On_Install'Access,
         Launch         => On_Launch'Access,
         Launch_Command => On_Launch_Command'Access,
         Metadata       => On_Metadata'Access);
   end Create_Server;

   -----------------------------
   -- On_Get_Launch_Interface --
   -----------------------------

   procedure On_Get_Launch_Interface
     (Parameters : in out Rose.Invocation.Invocation_Record)
   is
      Id : constant Rose.Objects.Capability_Identifier :=
        Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Exec.Library.Send_Interface (Id, Parameters);
   end On_Get_Launch_Interface;

   ----------------
   -- On_Install --
   ----------------

   function On_Install
     (Id        : Rose.Objects.Capability_Identifier;
      ELF_Image : Rose.Capabilities.Capability;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      use Rose.Interfaces.Stream_Reader.Client;
      Reader : Stream_Reader_Client;
   begin
      Open (Reader, ELF_Image);
      return Exec.Library.Install (Reader, Name, Caps);
   end On_Install;

   ---------------
   -- On_Launch --
   ---------------

   function On_Launch
     (Id          : Rose.Objects.Capability_Identifier;
      Caps        : Rose.Capabilities.Capability_Array;
      Environment : System.Storage_Elements.Storage_Array)
      return Rose.Capabilities.Capability
   is
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Create_Process_Cap);
      Send_Image (Params, Id);
      Exec.Library.Send_Install_Caps (Id, Params);

      for Cap of Caps loop
         Send_Cap (Params, Cap);
      end loop;

      Send_Cap (Params, Console_Cap);
      Rose.System_Calls.Send_Storage_Array (Params, Environment, False);

      Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return Rose.Capabilities.Null_Capability;
      else
         return Params.Caps (0);
      end if;
   end On_Launch;

   -----------------------
   -- On_Launch_Command --
   -----------------------

   function On_Launch_Command
     (Id   : Rose.Objects.Capability_Identifier;
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

      Initialize_Send (Params, Launch_Command_Cap);
      Send_Image (Params, Id);

      for Cap of Caps loop
         Send_Cap (Params, Cap);
      end loop;

      Exec.Library.Get_Program_Name (Id, Name, Last);
      Send_Text (Params, Name (1 .. Last));
      Receive_Caps (Params, 1);
      Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return Rose.Capabilities.Null_Capability;
      else
         return Params.Caps (0);
      end if;
   end On_Launch_Command;

   -----------------
   -- On_Metadata --
   -----------------

   procedure On_Metadata
     (Id     : Rose.Objects.Capability_Identifier;
      Result : out String;
      Last   : out Natural)
   is
   begin
      Exec.Library.Get_Metadata (Id, Result, Last);
   end On_Metadata;

   ----------------
   -- Send_Image --
   ----------------

   procedure Send_Image
     (Params : in out Rose.Invocation.Invocation_Record;
      Id     : Rose.Objects.Capability_Identifier)
   is
      Base, Bound   : Rose.Objects.Page_Object_Id;
      Subregion_Cap : Rose.Interfaces.Region.Client.Region_Client;
   begin
      Exec.Library.Get_Image_Pages (Id, Base, Bound);
      Subregion_Cap :=
        Rose.Interfaces.Region.Client.Create_Subregion
          (Region, Base, Bound, 5);
      Rose.System_Calls.Send_Cap
        (Params,
         Rose.Interfaces.Region.Client.Get_Interface_Cap (Subregion_Cap));
      Rose.System_Calls.Send_Cap (Params, Storage_Cap);
   end Send_Image;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Exec.Server;
