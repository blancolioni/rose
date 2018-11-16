with System.Storage_Elements;

with Rose.Capabilities;

package Rose.Devices.Block.Server is

   type Read_Block_Handler is access
     procedure (Block_Address : Block_Address_Type;
                Buffer        : out System.Storage_Elements.Storage_Array);

   type Write_Block_Handler is access
     procedure (Block_Address : Block_Address_Type;
                Buffer        : System.Storage_Elements.Storage_Array);

   procedure Run_Server
     (Endpoint_Cap  : Rose.Capabilities.Capability;
      Block_Size    : Block_Size_Type;
      Block_Count   : Block_Address_Type;
      Read_Handler  : Read_Block_Handler;
      Write_Handler : Write_Block_Handler);

end Rose.Devices.Block.Server;
