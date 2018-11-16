with Mem.Calls;

package body Mem.Virtual_Map is

   ---------
   -- Get --
   ---------

   function Get
     (Process : Rose.Objects.Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Virtual_Page_Mapping
   is (Virtual_Page_Mapping'
         (Valid         => True,
          Mapped        => True,
          Readable      => True,
          Writable      => True,
          Executable    => True,
          Write_Through => False,
          Disable_Cache => False,
          Unused        => False));

   ------------
   -- Create --
   ------------

   procedure Create (Mapping : Virtual_Page_Mapping) is null;

   ---------
   -- Map --
   ---------

   procedure Map (Process       : Rose.Objects.Process_Id;
                  Physical_Page : Rose.Addresses.Physical_Page_Address;
                  Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
                  Readable      : Boolean;
                  Writable      : Boolean;
                  Executable    : Boolean)
   is
   begin
      Mem.Calls.Map
        (Process    => Process,
         Physical   => Physical_Page,
         Virtual    => Virtual_Page,
         Readable   => Readable,
         Writeable  => Writable,
         Executable => Executable);
   end Map;

   ------------
   -- Remove --
   ------------

   procedure Remove (Mapping : Virtual_Page_Mapping) is null;

   -------------
   -- Reclaim --
   -------------

   procedure Reclaim
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean)
   is null;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All
     (Process : Rose.Objects.Process_Id)
   is null;

end Mem.Virtual_Map;
