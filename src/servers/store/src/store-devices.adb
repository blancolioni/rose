with Rose.Limits;
with Rose.Words;

with Rose.Console_IO;

with Rose.Interfaces.Block_Device.Client;

package body Store.Devices is

   Max_Backing_Stores : constant := 16;

   type Backing_Store_Record is
      record
         Allocated    : Boolean := False;
         Client       : Rose.Interfaces.Block_Device.Client
           .Block_Device_Client;
         Base         : Rose.Objects.Object_Id := 0;
         Bound        : Rose.Objects.Object_Id := 0;
         Device_Bound : Rose.Objects.Object_Id := 0;
      end record;

   type Backing_Store_Array is
     array (1 .. Max_Backing_Stores) of Backing_Store_Record;

   Backing_Stores      : Backing_Store_Array;
   Backing_Store_Count : Natural := 0;

   procedure New_Store
     (Client     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Page_Count : Rose.Words.Word);

   -----------------------
   -- Add_Backing_Store --
   -----------------------

   procedure Add_Backing_Store
     (Id     : in Rose.Objects.Capability_Identifier;
      Device : in Rose.Capabilities.Capability)
   is
      use type Rose.Objects.Capability_Identifier;
      use Rose.Interfaces.Block_Device, Rose.Interfaces.Block_Device.Client;
      Client : Block_Device_Client;
      Block_Count : Block_Address_Type;
      Block_Size  : Block_Size_Type;
   begin
      if Id /= 0 then
         Rose.Console_IO.Put
           ("storage: invalid id: ");
         Rose.Console_IO.Put (Natural (Id));
         Rose.Console_IO.New_Line;
      end if;

      Open (Client, Device);
      Get_Parameters (Client, Block_Count, Block_Size);
      if Block_Size /= Rose.Limits.Page_Size then
         Rose.Console_IO.Put ("storage: incorrect block size: expected ");
         Rose.Console_IO.Put (Natural (Rose.Limits.Page_Size));
         Rose.Console_IO.Put (" but found ");
         Rose.Console_IO.Put (Natural (Block_Size));
         Rose.Console_IO.New_Line;
         return;
      end if;

      Rose.Console_IO.Put ("storage: add backing store: size = ");
      Rose.Console_IO.Put (Natural (Block_Count) / 1024
                           * Natural (Block_Size)
                           / 1024);
      Rose.Console_IO.Put ("M");
      Rose.Console_IO.New_Line;
      New_Store (Client,
                 Rose.Words.Word (Block_Count));
   end Add_Backing_Store;

   ---------------
   -- New_Store --
   ---------------

   procedure New_Store
     (Client     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Page_Count : Rose.Words.Word)
   is
      use Rose.Objects;
      Index : Natural := 0;
      Found : Boolean := False;
      Request_Page_Count : Object_Id := 1;
   begin
      while Request_Page_Count < Object_Id (Page_Count) loop
         Request_Page_Count := Request_Page_Count * 2;
      end loop;

      if Backing_Store_Count = 0 then
         Backing_Store_Count := 1;
         Backing_Stores (1) := Backing_Store_Record'
           (Allocated    => False,
            Client       => <>,
            Base         => Rose.Objects.Page_Object_Id'First,
            Bound        => Rose.Objects.Page_Object_Id'Last,
            Device_Bound => Rose.Objects.Page_Object_Id'First);
      end if;

      while Index < Backing_Store_Count loop
         Index := Index + 1;
         declare
            Store : Backing_Store_Record renames
                      Backing_Stores (Index);
         begin
            if not Store.Allocated
              and then Store.Bound - Store.Base >= Request_Page_Count
            then
               Found := True;
               exit;
            end if;
         end;
      end loop;

      if not Found then
         Rose.Console_IO.Put ("no room for new backing store of size ");
         Rose.Console_IO.Put (Natural (Request_Page_Count) / 1024 / 1024);
         Rose.Console_IO.New_Line;
         return;
      end if;

      declare
         Store : Backing_Store_Record renames
                   Backing_Stores (Index);
      begin
         if Backing_Store_Count < Max_Backing_Stores
           and then Store.Bound - Store.Base > Request_Page_Count
         then
            Backing_Store_Count := Backing_Store_Count + 1;
            declare
               New_Free_Store : Backing_Store_Record renames
                                  Backing_Stores (Backing_Store_Count);
            begin
               New_Free_Store.Base := Store.Base + Request_Page_Count;
               New_Free_Store.Bound := Store.Bound;
               New_Free_Store.Allocated := False;
            end;
         end if;

         Store.Bound := Store.Base + Request_Page_Count;
         Store.Device_Bound := Store.Base + Object_Id (Page_Count);
         Store.Client := Client;
         Store.Allocated := True;

         Rose.Console_IO.Put ("new store: [");
         Rose.Console_IO.Put (Rose.Words.Word_64 (Store.Base));
         Rose.Console_IO.Put (", ");
         Rose.Console_IO.Put (Rose.Words.Word_64 (Store.Bound));
         Rose.Console_IO.Put (")");
         Rose.Console_IO.New_Line;
      end;

   end New_Store;

end Store.Devices;
