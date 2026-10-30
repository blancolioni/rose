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
   --  Reserve a region from the root bank.

   function Get_Root_Bank
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;
   --  The cap for the root bank, which every other bank is below.

   function Reserve_From_Bank
     (Id   : Rose.Objects.Capability_Identifier;
      Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability;
   --  Reserve a region from bank Id, and charge it to the bank and every
   --  bank above it.  Fails, with the null cap, if that would take one of
   --  them over a limit.

   function Create_Sub_Bank
     (Id           : Rose.Objects.Capability_Identifier;
      Page_Limit   : Rose.Words.Word_64;
      Region_Limit : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A bank below bank Id, with limits that are zero if it has none.

   procedure Get_Usage
     (Id           : in     Rose.Objects.Capability_Identifier;
      Pages        :    out Rose.Words.Word_64;
      Regions      :    out Rose.Words.Word;
      Page_Limit   :    out Rose.Words.Word_64;
      Region_Limit :    out Rose.Words.Word);

   procedure Destroy_Bank
     (Id : Rose.Objects.Capability_Identifier);
   --  Release every region charged to bank Id or below it, and destroy
   --  them.  The root bank stays.

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
      Base        : Rose.Objects.Object_Id;
      Bound       : Rose.Objects.Object_Id;
      Root        : Natural);
   --  Recreate a region from a checkpoint.  The backing stores must have
   --  been added again first, in the same order.

   procedure Restore_Table_Page
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Natural;
      Page  : System.Storage_Elements.Storage_Array);
   --  Read back page Index of the tables that Checkpoint appended, after
   --  the regions have been recreated: the blocks of the restored
   --  regions that had space on a device get it back.

   function Find_Region
     (Id    : Rose.Objects.Capability_Identifier;
      Base  : Rose.Objects.Object_Id;
      Bound : Rose.Objects.Object_Id)
      return Rose.Capabilities.Capability;
   --  The cap for the region whose pages are Base .. Bound - 1, or the
   --  null cap if there is none.

end Store.Devices;
