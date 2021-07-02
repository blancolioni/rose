with Rose.Objects;
with System.Storage_Elements;
with Rose.Interfaces.Stream_Reader.Client;
with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Region.Client is

   type Region_Client is private;

   procedure Open_Cap_Set
     (Client           :    out Region_Client;
      Get_Range        : in     Rose.Capabilities.Capability;
      Get              : in     Rose.Capabilities.Capability;
      Put              : in     Rose.Capabilities.Capability;
      Read             : in     Rose.Capabilities.Capability;
      Create_Subregion : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Region_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Get_Range
     (Item       : in     Region_Client;
      Base_Page  :    out Rose.Objects.Object_Id;
      Bound_Page :    out Rose.Objects.Object_Id);

   procedure Get
     (Item : in     Region_Client;
      Page : in     Rose.Objects.Object_Id;
      Data :    out System.Storage_Elements.Storage_Array);

   procedure Put
     (Item : Region_Client;
      Page : Rose.Objects.Object_Id;
      Data : System.Storage_Elements.Storage_Array);

   function Read (Item : Region_Client)
      return Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;

   function Create_Subregion
     (Item            : Region_Client;
      Subregion_Base  : Rose.Objects.Object_Id;
      Subregion_Bound : Rose.Objects.Object_Id;
      Flags           : Rose.Words.Word)
   return Rose.Interfaces.Region.Client.Region_Client;

   function Get_Interface_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Range_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability;

   function Get_Put_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability;

   function Get_Read_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability;

   function Get_Create_Subregion_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Region_Client is
      record
         Is_Open          : Boolean := False;
         Interface_Cap    : Rose.Capabilities.Capability := 0;
         Get_Range        : Rose.Capabilities.Capability := 0;
         Get              : Rose.Capabilities.Capability := 0;
         Put              : Rose.Capabilities.Capability := 0;
         Read             : Rose.Capabilities.Capability := 0;
         Create_Subregion : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Region.Client;
