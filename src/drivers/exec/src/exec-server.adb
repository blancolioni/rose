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
      Caps      : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;

   function On_Launch
     (Id          : Rose.Objects.Capability_Identifier;
      Caps        : Rose.Capabilities.Capability_Array;
      Environment : String)
      return Rose.Objects.Object_Id;

   Context : Rose.Server.Server_Context;
   Region : Rose.Interfaces.Region.Client.Region_Client;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Region.Client.Open (Region, Region_Cap);
      Exec.Library.Set_Region (Region);

      Rose.Interfaces.Exec.Server.Create_Server
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
      Caps      : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      use Rose.Interfaces.Stream_Reader.Client;
      Reader : Stream_Reader_Client;
   begin
      Open (Reader, ELF_Image);
      return Exec.Library.Install (Reader, Caps);
   end On_Install;

   ---------------
   -- On_Launch --
   ---------------

   function On_Launch
     (Id          : Rose.Objects.Capability_Identifier;
      Caps        : Rose.Capabilities.Capability_Array;
      Environment : String)
      return Rose.Objects.Object_Id
   is
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
      Base, Bound : Rose.Objects.Page_Object_Id;
      Subregion_Cap : Rose.Interfaces.Region.Client.Region_Client;
   begin

      Exec.Library.Get_Image_Pages (Id, Base, Bound);
      Subregion_Cap :=
        Rose.Interfaces.Region.Client.Create_Subregion
          (Region, Base, Bound, 1);

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
      Rose.System_Calls.Send_Text (Params, Environment);

      Invoke_Capability (Params);

      return Rose.Objects.Object_Id
        (Rose.System_Calls.Get_Word_64
           (Params, 0));
   end On_Launch;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Exec.Server;
