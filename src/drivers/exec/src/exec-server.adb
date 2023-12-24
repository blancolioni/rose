with System.Storage_Elements;

with Rose.Invocation;
with Rose.Objects;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Exec.Server;
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

      Rose.Interfaces.Exec.Server.Publish_Interface
        (Server_Context => Context,
         Install        => On_Install'Access,
         Launch         => On_Launch'Access);
   end Create_Server;

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
      Base, Bound : Rose.Objects.Page_Object_Id;
      Subregion_Cap : Rose.Interfaces.Region.Client.Region_Client;
   begin

      Exec.Library.Get_Image_Pages (Id, Base, Bound);
      Subregion_Cap :=
        Rose.Interfaces.Region.Client.Create_Subregion
          (Region, Base, Bound, 5);

      Initialize_Send (Params, Create_Process_Cap);
      Send_Cap
        (Params,
         Rose.Interfaces.Region.Client.Get_Interface_Cap (Subregion_Cap));
      Send_Cap (Params, Storage_Cap);

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

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Exec.Server;
