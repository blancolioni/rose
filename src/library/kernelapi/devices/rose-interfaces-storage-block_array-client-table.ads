generic
   Size : Natural;
   type Element_Type is private;
package Rose.Interfaces.Storage.Block_Array.Client.Table is

   procedure Insert
     (Device : Block_Array_Client;
      Element : Element_Type);

   procedure Update
     (Device  : Block_Array_Client;
      Element : Element_Type);

   function Contains
     (Device : Block_Array_Client)
      return Boolean;

   procedure Get_Element
     (Device : Block_Array_Client;
      Element : out Element_Type);

end Rose.Interfaces.Storage.Block_Array.Client.Table;
