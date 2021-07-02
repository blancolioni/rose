with Rose.Words;
with Rose.Interfaces.Region.Client;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Segment.Client is

   type Segment_Client is private;

   procedure Open_Cap_Set
     (Client                    :    out Segment_Client;
      Add_Segment               : in     Rose.Capabilities.Capability;
      Add_Nonpersistent_Segment : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Segment_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Add_Segment
     (Item          : Segment_Client;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Interfaces.Region.Client.Region_Client;
      Region_Offset : Rose.Words.Word;
      Flags         : Rose.Words.Word);

   procedure Add_Nonpersistent_Segment
     (Item          : Segment_Client;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Flags         : Rose.Words.Word);

   function Get_Interface_Cap (Item : Segment_Client)
      return Rose.Capabilities.Capability;

   function Get_Add_Segment_Cap (Item : Segment_Client)
      return Rose.Capabilities.Capability;

   function Get_Add_Nonpersistent_Segment_Cap (Item : Segment_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Segment_Client is
      record
         Is_Open                   : Boolean := False;
         Interface_Cap             : Rose.Capabilities.Capability := 0;
         Add_Segment               : Rose.Capabilities.Capability := 0;
         Add_Nonpersistent_Segment : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Segment.Client;
