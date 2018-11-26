generic
   Size : Natural;
   type Element_Type is private;
package Rose.Interfaces.Block_Device.Client.Table is

   procedure Insert
     (Device : Block_Device_Client;
      Element : Element_Type);

   procedure Update
     (Device  : Block_Device_Client;
      Element : Element_Type);

   function Contains
     (Device : Block_Device_Client)
      return Boolean;

   procedure Get_Element
     (Device : Block_Device_Client;
      Element : out Element_Type);

end Rose.Interfaces.Block_Device.Client.Table;
