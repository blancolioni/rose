with Rose.Addresses;
with Rose.Objects;

package Mem.Virtual_Map is

   type Virtual_Page_Mapping is private;

   function Get
     (Process : Rose.Objects.Capability_Identifier;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Virtual_Page_Mapping;

   procedure Create (Mapping : Virtual_Page_Mapping);
   procedure Remove (Mapping : Virtual_Page_Mapping);

   procedure Reclaim
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean);

   procedure Map (Process       : Rose.Objects.Object_Id;
                  Physical_Page : Rose.Addresses.Physical_Page_Address;
                  Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
                  Readable      : Boolean;
                  Writable      : Boolean;
                  Executable    : Boolean);

   function Valid (Mapping : Virtual_Page_Mapping) return Boolean;
   function Mapped (Mapping : Virtual_Page_Mapping) return Boolean;
   function Readable (Mapping : Virtual_Page_Mapping) return Boolean;
   function Writable (Mapping : Virtual_Page_Mapping) return Boolean;
   function Executable (Mapping : Virtual_Page_Mapping) return Boolean;

   procedure Set_Read_Only (Mapping : Virtual_Page_Mapping);
   procedure Set_Read_Write (Mapping : Virtual_Page_Mapping);
   procedure Set_Executable (Mapping : Virtual_Page_Mapping);

   procedure Remove_All
     (Process : Rose.Objects.Capability_Identifier);

private

   type Virtual_Page_Mapping is
      record
         Valid         : Boolean;
         Mapped        : Boolean;
         Readable      : Boolean;
         Writable      : Boolean;
         Executable    : Boolean;
         Write_Through : Boolean;
         Disable_Cache : Boolean;
         Unused        : Boolean;
      end record
   with Pack, Size => 8;

   function Valid (Mapping : Virtual_Page_Mapping) return Boolean
   is (Mapping.Valid);

   function Mapped (Mapping : Virtual_Page_Mapping) return Boolean
   is (Mapping.Mapped);

   function Readable (Mapping : Virtual_Page_Mapping) return Boolean
   is (Mapping.Readable);

   function Writable (Mapping : Virtual_Page_Mapping) return Boolean
   is (Mapping.Writable);

   function Executable (Mapping : Virtual_Page_Mapping) return Boolean
   is (Mapping.Executable);

   procedure Set_Read_Only (Mapping : Virtual_Page_Mapping) is null;
   procedure Set_Read_Write (Mapping : Virtual_Page_Mapping) is null;
   procedure Set_Executable (Mapping : Virtual_Page_Mapping) is null;
end Mem.Virtual_Map;
