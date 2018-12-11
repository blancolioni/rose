with System.Storage_Elements;

with Rose.Console_IO;
with Rose.System_Calls.Server;
with Rose.Limits;

with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Process_Memory;

with Mem.Calls;

package body Mem.Processes is

   type Process_State is (Available, Active, Faulted);

   type Segment_Flag is (Read, Write, Execute, Extensible, Persistent);

   type Segment_Flag_Array is array (Segment_Flag) of Boolean;

   subtype Page_Buffer is
     System.Storage_Elements.Storage_Array (1 .. Rose.Limits.Page_Size);

   Buffer : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;

   Max_Segments : constant := 8;
   type Segment_Count is range 0 .. Max_Segments;
   subtype Segment_Index is Segment_Count range 1 .. Segment_Count'Last;

   type Segment_Record is
      record
         Base         : Rose.Addresses.Virtual_Page_Address := 0;
         Bound        : Rose.Addresses.Virtual_Page_Address := 0;
         Region       : Rose.Interfaces.Region.Client.Region_Client;
         Region_Base  : Rose.Objects.Object_Id := 0;
         Region_Bound : Rose.Objects.Object_Id := 0;
         Flags        : Segment_Flag_Array := (others => False);
      end record;

   type Segment_Record_Array is array (Segment_Index) of Segment_Record;

   type Memory_Process_Record is
      record
         State        : Process_State := Available;
         Oid          : Rose.Objects.Object_Id;
         Segments     : Segment_Record_Array;
         Num_Segments : Segment_Count;
         Process      : Rose.Interfaces.Process.Client.Process_Client;
      end record;

   subtype Real_Process_Id is
     Rose.Objects.Capability_Identifier range 1 .. Rose.Limits.Max_Processes;

   type Memory_Process_Table is
     array (Real_Process_Id) of aliased Memory_Process_Record;

   Process_Table : Memory_Process_Table;
   Next_Pid      : Real_Process_Id := 1;

   -------------------------------
   -- Add_Nonpersistent_Segment --
   -------------------------------

   procedure Add_Nonpersistent_Segment
     (Process        : Rose.Objects.Capability_Identifier;
      Virtual_Base   : Rose.Addresses.Virtual_Address;
      Virtual_Bound  : Rose.Addresses.Virtual_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean)
   is
      use Rose.Addresses;
      P : Memory_Process_Record renames Process_Table (Process);
      Virtual_Page_Base : constant Virtual_Page_Address :=
                            Virtual_Address_To_Page
                              (Virtual_Base);
      Virtual_Page_Bound : constant Virtual_Page_Address :=
                            Virtual_Address_To_Page
                               (Virtual_Bound);
   begin
      if P.Num_Segments = Max_Segments then
         return;
      end if;

      P.Num_Segments := P.Num_Segments + 1;
      P.Segments (P.Num_Segments) := Segment_Record'
        (Base         => Virtual_Page_Base,
         Bound        => Virtual_Page_Bound,
         Region       => <>,
         Region_Base  => 0,
         Region_Bound => 0,
         Flags        =>
           (Read       => Readable,
            Write      => Writable,
            Execute    => Executable,
            Extensible => False,
            Persistent => False));

   end Add_Nonpersistent_Segment;

   -----------------
   -- Add_Segment --
   -----------------

   procedure Add_Segment
     (Process       : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Addresses.Virtual_Address;
      Region        : Rose.Interfaces.Region.Client.Region_Client;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean)
   is
      use Rose.Addresses;
      use type Rose.Objects.Object_Id;
      P : Memory_Process_Record renames Process_Table (Process);
      Region_Base : Rose.Objects.Page_Object_Id;
      Region_Bound : Rose.Objects.Page_Object_Id;
      Virtual_Page_Base : constant Virtual_Page_Address :=
                            Virtual_Address_To_Page
                              (Virtual_Base);
   begin
      if P.Num_Segments = Max_Segments then
         return;
      end if;

      Rose.Interfaces.Region.Client.Get_Range
        (Region, Region_Base, Region_Bound);

      P.Num_Segments := P.Num_Segments + 1;
      P.Segments (P.Num_Segments) := Segment_Record'
        (Base         => Virtual_Page_Base,
         Bound        => Virtual_Page_Base +
           Virtual_Page_Address (Region_Bound - Region_Base),
         Region       => Region,
         Region_Base  => Region_Base,
         Region_Bound => Region_Bound,
         Flags        =>
           (Read => Readable,
            Write => Writable,
            Execute => Executable,
            Extensible => False,
            Persistent => True));

   end Add_Segment;

   -------------------
   -- Fault_Process --
   -------------------

   procedure Fault_Process
     (Process         : Rose.Objects.Capability_Identifier)
   is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Rose.Interfaces.Process.Client.Fault
        (P.Process);
      P.State := Faulted;
   end Fault_Process;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id
     (Process : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id
   is
   begin
      if Process not in Real_Process_Id
        or else Process_Table (Process).State = Available
      then
         return Rose.Objects.Null_Object_Id;
      else
         return Process_Table (Process).Oid;
      end if;
   end Get_Object_Id;

   --------------------
   -- Get_Process_Id --
   --------------------

   function Get_Process_Id
     (Process : Rose.Objects.Object_Id)
      return Rose.Objects.Capability_Identifier
   is
      use type Rose.Objects.Object_Id;
   begin
      for I in Process_Table'Range loop
         if Process_Table (I).State = Active
           and then Process_Table (I).Oid = Process
         then
            return I;
         end if;
      end loop;
      return 0;
   end Get_Process_Id;

   -------------------------
   -- Get_Process_Segment --
   -------------------------

   procedure Get_Process_Segment
     (Process         : Rose.Objects.Capability_Identifier;
      Virtual_Page    : Rose.Addresses.Virtual_Page_Address;
      Page_Object     : out Rose.Objects.Object_Id;
      Valid           : out Boolean;
      Readable        : out Boolean;
      Writable        : out Boolean;
      Executable      : out Boolean)
   is
      use Rose.Addresses;
      use type Rose.Objects.Object_Id;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Virtual_Page >= Segment.Base
           and then Virtual_Page < Segment.Bound
         then
            Valid := True;
            Readable := Segment.Flags (Read);
            Writable := Segment.Flags (Write);
            Executable := Segment.Flags (Execute);
            if Segment.Region_Base /= 0 then
               Page_Object := Segment.Region_Base
                 + Rose.Objects.Object_Id (Virtual_Page - Segment.Base);
            else
               Page_Object := 0;
            end if;
            return;
         end if;
      end loop;

      Valid := False;
   end Get_Process_Segment;

   ---------------------
   -- Initialize_Page --
   ---------------------

   procedure Initialize_Page
     (Process       : Rose.Objects.Capability_Identifier;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address)
   is
      use Rose.Addresses;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Virtual_Page >= Segment.Base
           and then Virtual_Page < Segment.Bound
         then

            Mem.Calls.Load_Page (Physical_Page, Buffer'Address);

            if Segment.Flags (Persistent) then
               declare
                  use Rose.Objects;
                  Page : constant Page_Object_Id :=
                           Segment.Region_Base
                             + Object_Id (Virtual_Page - Segment.Base);
               begin
                  Rose.Interfaces.Region.Client.Get
                    (Item => Segment.Region,
                     Page => Page,
                     Data => Buffer);
               end;
            else
               Buffer := (others => 0);
            end if;

            Mem.Calls.Unload_Page (Physical_Page, Buffer'Address);

            return;
         end if;
      end loop;
   end Initialize_Page;

   -------------------------
   -- Is_Valid_Process_Id --
   -------------------------

   function Is_Valid_Process_Id
     (Process : Rose.Objects.Capability_Identifier)
      return Boolean
   is
   begin
      return Process in Real_Process_Id
        and then Process_Table (Process).State = Active;
   end Is_Valid_Process_Id;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Process : Rose.Objects.Capability_Identifier) is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Rose.Interfaces.Process.Client.Destroy (P.Process);
      P.State := Available;
   end Kill_Process;

   -----------------
   -- New_Process --
   -----------------

   function New_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      use Rose.Objects;
      Start : constant Capability_Identifier := Next_Pid;
      Client : Rose.Interfaces.Process.Client.Process_Client;
   begin
      while Process_Table (Next_Pid).State /= Available loop
         if Next_Pid = Process_Table'Last then
            Next_Pid := Process_Table'First;
         else
            Next_Pid := Next_Pid + 1;
         end if;
         if Next_Pid = Start then
            Rose.Console_IO.Put_Line ("out of processes");
            return Rose.Capabilities.Null_Capability;
         end if;
      end loop;

      Rose.Interfaces.Process.Client.Open (Client, Process_Cap);
      Process_Table (Next_Pid) :=
        Memory_Process_Record'
          (State    => Active,
           Oid      => Rose.Interfaces.Process.Client.Get_Object_Id (Client),
           Segments => <>,
           Num_Segments => 0,
           Process  => Client);

      return Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap   => Create_Endpoint_Cap,
         Endpoint_Id  =>
           Rose.Interfaces.Process_Memory.Process_Memory_Interface,
         Identifier   => Next_Pid);

   end New_Process;

   --------------------
   -- Resume_Process --
   --------------------

   procedure Resume_Process
     (Process         : Rose.Objects.Capability_Identifier)
   is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Rose.Interfaces.Process.Client.Resume (P.Process);
   end Resume_Process;

end Mem.Processes;
