with System.Storage_Elements;

with Rose.Objects;
with Rose.Capabilities;

package Rose.Devices.Block.Server is

   type Get_Parameters_Handler is access
     procedure (Identifier    : Rose.Objects.Capability_Identifier;
                Block_Size    : out Block_Size_Type;
                Block_Count   : out Block_Address_Type);

   type Read_Block_Handler is access
     procedure (Identifier    : Rose.Objects.Capability_Identifier;
                Block_Address : Block_Address_Type;
                Buffer        : out System.Storage_Elements.Storage_Array);

   type Write_Block_Handler is access
     procedure (Identifier    : Rose.Objects.Capability_Identifier;
                Block_Address : Block_Address_Type;
                Buffer        : System.Storage_Elements.Storage_Array);

   type Get_Interface_Handler is access
     procedure (Identifier         : Rose.Objects.Capability_Identifier;
                Get_Parameters_Cap : out Rose.Capabilities.Capability;
                Read_Block_Cap     : out Rose.Capabilities.Capability;
                Write_Block_Cap    : out Rose.Capabilities.Capability);

   procedure Run_Server
     (Endpoint_Cap   : Rose.Capabilities.Capability;
      Get_Parameters : Get_Parameters_Handler;
      Read_Handler   : Read_Block_Handler;
      Write_Handler  : Write_Block_Handler;
      Get_Interface  : Get_Interface_Handler);

end Rose.Devices.Block.Server;
