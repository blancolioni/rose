generic
   Size : Natural;
   type Element_Type is private;
package Rose.Devices.Block.Client.Table is

   procedure Insert
     (Device : Block_Device_Type;
      Element : Element_Type);

   procedure Update
     (Device  : Block_Device_Type;
      Element : Element_Type);

   function Contains
     (Device : Block_Device_Type)
      return Boolean;

   procedure Get_Element
     (Device : Block_Device_Type;
      Element : out Element_Type);

end Rose.Devices.Block.Client.Table;
