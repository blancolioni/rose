with System.Storage_Elements;

with Rose.Capabilities;

package Rose.Devices.Block.Client is

   type Block_Device_Type is private;

   procedure Open
     (Device : in out Block_Device_Type;
      Parameters_Cap : Rose.Capabilities.Capability;
      Read_Cap       : Rose.Capabilities.Capability;
      Write_Cap      : Rose.Capabilities.Capability);

   function Get_Block_Count
     (Device : Block_Device_Type)
      return Block_Address_Type;

   function Get_Block_Size
     (Device : Block_Device_Type)
      return Block_Size_Type;

   procedure Read_Block
     (Device        : Block_Device_Type;
      Block_Address : Block_Address_Type;
      Block_Storage : out System.Storage_Elements.Storage_Array);

   procedure Write_Block
     (Device        : Block_Device_Type;
      Block_Address : Block_Address_Type;
      Block_Storage : System.Storage_Elements.Storage_Array);

private

   type Block_Device_Type is
      record
         Open                    : Boolean := False;
         Parameters, Read, Write : Rose.Capabilities.Capability;
         Block_Size              : Block_Size_Type;
         Block_Count             : Block_Address_Type;
      end record;

end Rose.Devices.Block.Client;
