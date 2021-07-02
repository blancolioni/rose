with Rose.Server;
with Rose.Interfaces.Block_Device;
with System.Storage_Elements;

package Rose.Interfaces.Block_Device.Server is

   type Get_Parameters_Handler is access
     procedure
       (Id          : in     Rose.Objects.Capability_Identifier;
        Block_Count :    out Block_Address_Type;
        Block_Size  :    out Rose.Interfaces.Block_Device.Block_Size_Type);

   type Read_Blocks_Handler is access
     procedure
       (Id     : in     Rose.Objects.Capability_Identifier;
        Start  : in     Block_Address_Type;
        Count  : in     Natural;
        Blocks :    out System.Storage_Elements.Storage_Array);

   type Write_Blocks_Handler is access
     procedure
       (Id     : Rose.Objects.Capability_Identifier;
        Start  : Block_Address_Type;
        Count  : Natural;
        Blocks : System.Storage_Elements.Storage_Array);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Parameters : in     Get_Parameters_Handler;
      Read_Blocks    : in     Read_Blocks_Handler;
      Write_Blocks   : in     Write_Blocks_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Parameters : in     Get_Parameters_Handler;
      Read_Blocks    : in     Read_Blocks_Handler;
      Write_Blocks   : in     Write_Blocks_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Parameters : in     Get_Parameters_Handler;
      Read_Blocks    : in     Read_Blocks_Handler;
      Write_Blocks   : in     Write_Blocks_Handler);

private

end Rose.Interfaces.Block_Device.Server;
