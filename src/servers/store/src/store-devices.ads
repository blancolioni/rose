with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;

package Store.Devices is

   procedure Add_Backing_Store
     (Id     : in Rose.Objects.Capability_Identifier;
      Device : in Rose.Capabilities.Capability);

   function Reserve_Storage
     (Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability;

   procedure Get_Range
     (Id    : Rose.Objects.Capability_Identifier;
      Base  : out Rose.Objects.Object_Id;
      Bound : out Rose.Objects.Object_Id);

   procedure Get
     (Id   : in     Rose.Objects.Capability_Identifier;
      Page : in     Rose.Objects.Object_Id;
      Data :    out System.Storage_Elements.Storage_Array);

   procedure Put
     (Id   : in     Rose.Objects.Capability_Identifier;
      Page : in     Rose.Objects.Object_Id;
      Data : in     System.Storage_Elements.Storage_Array);

   function Read_Stream
     (Id      : in Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Create_Subregion
     (Id      : in Rose.Objects.Capability_Identifier;
      Base    : Rose.Objects.Object_Id;
      Bound   : Rose.Objects.Object_Id;
      Flags   : Rose.Words.Word)
      return Rose.Capabilities.Capability;

   procedure Release
     (Id : Rose.Objects.Capability_Identifier);
   --  Free the region Id, and its storage if it has its own.

   procedure Read
     (Id      : in Rose.Objects.Capability_Identifier;
      Storage : out System.Storage_Elements.Storage_Array;
      Last    : out System.Storage_Elements.Storage_Count);

   procedure Checkpoint
     (Id          : Rose.Objects.Capability_Identifier;
      Append_Page : Rose.Capabilities.Capability);
   --  Append the region table to the checkpoint log through Append_Page,
   --  in the format of Rose.Checkpoints, then delete Append_Page.

   procedure Restore_Region
     (Id          : Rose.Objects.Capability_Identifier;
      Index       : Natural;
      Device      : Natural;
      Base        : Rose.Objects.Object_Id;
      Bound       : Rose.Objects.Object_Id;
      Alloc_Index : Natural;
      Alloc_Banks : Natural);
   --  Recreate a region from a checkpoint.  The backing stores must have
   --  been added again first, in the same order.

   procedure Restore_Table_Page
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Natural;
      Page  : System.Storage_Elements.Storage_Array);
   --  Read back page Index of the tables that Checkpoint appended, after
   --  the regions have been recreated: the fresh pages of the restored
   --  regions are marked fresh again.

   function Find_Region
     (Id    : Rose.Objects.Capability_Identifier;
      Base  : Rose.Objects.Object_Id;
      Bound : Rose.Objects.Object_Id)
      return Rose.Capabilities.Capability;
   --  The cap for the region whose pages are Base .. Bound - 1, or the
   --  null cap if there is none.

end Store.Devices;
