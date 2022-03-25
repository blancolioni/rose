with System.Storage_Elements;

with Rose.Limits;

with Mem.Calls;
with Mem.Page_Table;
with Mem.Processes;

package body Mem.Virtual_Map is

   Page_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;

   ---------
   -- Map --
   ---------

   procedure Map (Process       : Rose.Objects.Object_Id;
                  Physical_Page : Rose.Addresses.Physical_Page_Address;
                  Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
                  Readable      : Boolean;
                  Writable      : Boolean;
                  Executable    : Boolean;
                  Persistent    : Boolean)
   is
      pragma Unreferenced (Writable);
   begin
      Mem.Calls.Map
        (Process    => Process,
         Physical   => Physical_Page,
         Virtual    => Virtual_Page,
         Readable   => Readable,
         Writeable  => False,
         Executable => Executable);
      Mem.Page_Table.Insert
        (Process    => Mem.Processes.Get_Process_Id (Process),
         Physical   => Physical_Page,
         Virtual    => Virtual_Page,
         Mapped     => True,
         Readable   => True,
         Writable   => False,
         Executable => Executable,
         Persistent => Persistent);
   end Map;

   -------------
   -- Reclaim --
   -------------

   procedure Reclaim
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean)
   is
   begin
      Physical_Page := 0;
      Success := False;
   end Reclaim;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All
     (Process : Rose.Objects.Capability_Identifier)
   is null;

   ---------------
   -- With_Page --
   ---------------

   procedure With_Page
     (Page    : Rose.Addresses.Physical_Page_Address;
      Process : not null access
        procedure (Address : System.Address))
   is
   begin
      Mem.Calls.Load_Page (Page, Page_Buffer'Address);
      Process (Page_Buffer'Address);
      Mem.Calls.Unload_Page (Page, Page_Buffer'Address);
   end With_Page;

end Mem.Virtual_Map;
