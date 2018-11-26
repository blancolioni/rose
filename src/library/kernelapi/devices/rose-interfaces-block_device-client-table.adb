package body Rose.Interfaces.Block_Device.Client.Table is

   type Table_Entry_Record is
      record
         Parameters_Cap : Rose.Capabilities.Capability :=
                            Rose.Capabilities.Null_Capability;
         Element        : Element_Type;
      end record;

   Table_Array : array (1 .. Size) of Table_Entry_Record;
   Table_Count : Natural := 0;

   function Find (Device : Block_Device_Client) return Natural;

   --------------
   -- Contains --
   --------------

   function Contains
     (Device : Block_Device_Client)
      return Boolean
   is
   begin
      return Find (Device) > 0;
   end Contains;

   ----------
   -- Find --
   ----------

   function Find (Device : Block_Device_Client) return Natural is
      use type Rose.Capabilities.Capability;
   begin
      for I in 1 .. Table_Count loop
         if Table_Array (I).Parameters_Cap = Device.Get_Parameters then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   -----------------
   -- Get_Element --
   -----------------

   procedure Get_Element
     (Device  : Block_Device_Client;
      Element : out Element_Type)
   is
      Index : constant Natural := Find (Device);
   begin
      if Index > 0 then
         Element := Table_Array (Index).Element;
      end if;
   end Get_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Device : Block_Device_Client;
      Element : Element_Type)
   is
   begin
      if Table_Count < Size then
         Table_Count := Table_Count + 1;
         Table_Array (Table_Count) := (Device.Get_Parameters, Element);
      end if;
   end Insert;

   ------------
   -- Update --
   ------------

   procedure Update
     (Device  : Block_Device_Client;
      Element : Element_Type)
   is
      Index : constant Natural := Find (Device);
   begin
      if Index > 0 then
         Table_Array (Index).Element := Element;
      end if;
   end Update;

end Rose.Interfaces.Block_Device.Client.Table;
