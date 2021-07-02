with System.Storage_Elements;
with Rose.Capabilities;
with Rose.Invocation;
with Rose.Interfaces.Block_Device;

package Rose.Interfaces.Block_Device.Client is

   type Block_Device_Client is private;

   procedure Open_Cap_Set
     (Client         :    out Block_Device_Client;
      Get_Parameters : in     Rose.Capabilities.Capability;
      Read_Blocks    : in     Rose.Capabilities.Capability;
      Write_Blocks   : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Block_Device_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Get_Parameters
     (Item        : in     Block_Device_Client;
      Block_Count :    out Block_Address_Type;
      Block_Size  :    out Rose.Interfaces.Block_Device.Block_Size_Type);

   procedure Read_Blocks
     (Item   : in     Block_Device_Client;
      Start  : in     Block_Address_Type;
      Count  : in     Natural;
      Blocks :    out System.Storage_Elements.Storage_Array);

   procedure Write_Blocks
     (Item   : Block_Device_Client;
      Start  : Block_Address_Type;
      Count  : Natural;
      Blocks : System.Storage_Elements.Storage_Array);

   function Get_Interface_Cap (Item : Block_Device_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Parameters_Cap (Item : Block_Device_Client)
      return Rose.Capabilities.Capability;

   function Get_Read_Blocks_Cap (Item : Block_Device_Client)
      return Rose.Capabilities.Capability;

   function Get_Write_Blocks_Cap (Item : Block_Device_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Block_Device_Client is
      record
         Is_Open        : Boolean := False;
         Interface_Cap  : Rose.Capabilities.Capability := 0;
         Get_Parameters : Rose.Capabilities.Capability := 0;
         Read_Blocks    : Rose.Capabilities.Capability := 0;
         Write_Blocks   : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Block_Device.Client;
