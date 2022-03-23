with Mem.Calls;
with Mem.Page_Table;
with Mem.Processes;

package body Mem.Virtual_Map is

   ---------
   -- Map --
   ---------

   procedure Map (Process       : Rose.Objects.Object_Id;
                  Physical_Page : Rose.Addresses.Physical_Page_Address;
                  Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
                  Readable      : Boolean;
                  Writable      : Boolean;
                  Executable    : Boolean)
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
         Executable => Executable);
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

end Mem.Virtual_Map;
