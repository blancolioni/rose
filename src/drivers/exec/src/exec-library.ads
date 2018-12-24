with Rose.Invocation;
with Rose.Objects;

with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Stream_Reader.Client;

package Exec.Library is

   procedure Set_Region
     (Client : Rose.Interfaces.Region.Client.Region_Client);

   function Install
     (ELF_Image  : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Caps       : Rose.Capabilities.Capability_Array)
     return Rose.Capabilities.Capability;

   procedure Get_Image_Pages
     (Id          : Rose.Objects.Capability_Identifier;
      Base, Bound : out Rose.Objects.Page_Object_Id);

   procedure Send_Install_Caps
     (Id     : Rose.Objects.Capability_Identifier;
      Params : in out Rose.Invocation.Invocation_Record);

end Exec.Library;
