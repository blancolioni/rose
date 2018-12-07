with Rose.Limits;
with Rose.Allocators;

with Rose.Console_IO;
with Rose.System_Calls.Server;

with Rose.Interfaces.Block_Device.Client;

package body Store.Devices is

   Max_Backing_Stores : constant := 16;
   Max_Space_Banks    : constant := 256;
   Minimum_Bank_Size  : constant := 2 ** 4;
   Maximum_Store_Size : constant := 2 ** 20;

   type Backing_Store_Record is
      record
         Allocated       : Boolean := False;
         Client          : Rose.Interfaces.Block_Device.Client
           .Block_Device_Client;
         Base            : Rose.Objects.Object_Id := 0;
         Bound           : Rose.Objects.Object_Id := 0;
         Device_Bound    : Rose.Objects.Object_Id := 0;
         Allocator_Base  : Positive;
         Allocator_Bound : Positive;
      end record;

   type Backing_Store_Array is
     array (1 .. Max_Backing_Stores) of Backing_Store_Record;

   Backing_Stores      : Backing_Store_Array;
   Backing_Store_Count : Natural := 0;

   type Space_Bank_Record is
      record
         Backing_Device : Natural := 0;
         Base, Bound    : Rose.Objects.Object_Id := 0;
         Access_Cap     : Rose.Capabilities.Capability := 0;
      end record;

   type Space_Bank_Array is
     array (1 .. Max_Space_Banks) of Space_Bank_Record;

   Space_Banks      : Space_Bank_Array;
   Space_Bank_Count : Natural := 0;

   Allocator : Rose.Allocators.Store (Maximum_Store_Size / Minimum_Bank_Size);

   procedure New_Store
     (Client     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Page_Count : Rose.Words.Word;
      Base       : out Rose.Objects.Object_Id;
      Bound      : out Rose.Objects.Object_Id);

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

      declare
         use Rose.Objects;
         Base, Bound : Rose.Objects.Object_Id;
      begin
         New_Store (Client,
                    Rose.Words.Word (Block_Count),
                    Base, Bound);

         Rose.Allocators.Add_Available_Capacity
           (Allocator => Allocator,
            Base      =>
              Natural ((Base - Page_Object_Id'First) / Minimum_Bank_Size),
            Bound     =>
              Natural ((Bound - Page_Object_Id'First) / Minimum_Bank_Size));
      end;

   end Add_Backing_Store;

   ---------------
   -- Get_Range --
   ---------------

   procedure Get_Range
     (Id    : Rose.Objects.Capability_Identifier;
      Base  : out Rose.Objects.Object_Id;
      Bound : out Rose.Objects.Object_Id)
   is
      Space_Bank_Index : constant Natural := Natural (Id);
   begin
      if Space_Bank_Index not in 1 .. Space_Bank_Count then
         Base := 0;
         Bound := 0;
         return;
      end if;

      Base := Space_Banks (Space_Bank_Index).Base;
      Bound := Space_Banks (Space_Bank_Index).Bound;
   end Get_Range;

   ---------------
   -- New_Store --
   ---------------

   procedure New_Store
     (Client     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Page_Count : Rose.Words.Word;
      Base       : out Rose.Objects.Object_Id;
      Bound      : out Rose.Objects.Object_Id)
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
           (Allocated       => False,
            Client          => <>,
            Base            => Rose.Objects.Page_Object_Id'First,
            Bound           => Rose.Objects.Page_Object_Id'Last,
            Device_Bound    => Rose.Objects.Page_Object_Id'First,
            Allocator_Base  => 1,
            Allocator_Bound => Positive'Last);
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
         Base := 0;
         Bound := 0;

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

         Base := Store.Base;
         Bound := Store.Bound;

         Rose.Console_IO.Put ("new store: [");
         Rose.Console_IO.Put (Rose.Words.Word_64 (Store.Base));
         Rose.Console_IO.Put (", ");
         Rose.Console_IO.Put (Rose.Words.Word_64 (Store.Bound));
         Rose.Console_IO.Put (")");
         Rose.Console_IO.New_Line;
      end;

   end New_Store;

   function Reserve_Storage
     (Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability
   is
      Alloc_Index : Natural;
      Alloc_Size  : Natural := Natural (Size);
   begin
      if Space_Bank_Count >= Max_Space_Banks then
         return 0;
      end if;

      if Alloc_Size < Minimum_Bank_Size then
         Alloc_Size := Minimum_Bank_Size;
      elsif Alloc_Size mod Minimum_Bank_Size /= 0 then
         Alloc_Size := (Alloc_Size / Minimum_Bank_Size + 1)
           * Minimum_Bank_Size;
      end if;

      Alloc_Index :=
        Rose.Allocators.Allocate
          (Allocator, Alloc_Size / Minimum_Bank_Size);

      if Alloc_Index = 0 then
         return 0;
      end if;

      Space_Bank_Count := Space_Bank_Count + 1;
      declare
         use Rose.Objects;
         New_Bank : Space_Bank_Record renames
                      Space_Banks (Space_Bank_Count);
         Store    : Positive := 1;
         Base     : Object_Id;
         Bound    : Object_Id;
      begin
         while Backing_Stores (Store).Allocator_Bound <= Alloc_Index loop
            Store := Store + 1;
         end loop;

         Base :=
           Object_Id
             ((Alloc_Index - Backing_Stores (Store).Allocator_Base)
              * Minimum_Bank_Size);
         Bound :=
           Base + Object_Id (Alloc_Size * Minimum_Bank_Size);

         New_Bank := Space_Bank_Record'
           (Backing_Device => Store,
            Base           => Base,
            Bound          => Bound,
            Access_Cap     =>
              Rose.System_Calls.Server.Create_Endpoint
                (Create_Endpoint_Cap,
                 Rose.Interfaces.Space_Bank.Get_Range_Endpoint,
                 Capability_Identifier (Space_Bank_Count)));

         return New_Bank.Access_Cap;
      end;
   end Reserve_Storage;

end Store.Devices;
