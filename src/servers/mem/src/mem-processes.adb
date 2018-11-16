package body Mem.Processes is

   -------------------------
   -- Get_Process_Segment --
   -------------------------

   procedure Get_Process_Segment
     (Process         : Rose.Objects.Process_Id;
      Virtual_Address : Rose.Addresses.Virtual_Page_Address;
      Valid           : out Boolean;
      Readable        : out Boolean;
      Writable        : out Boolean;
      Executable      : out Boolean)
   is
      use Rose.Addresses;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Virtual_Address >= Segment.Base
           and then Virtual_Address < Segment.Bound
         then
            Valid := True;
            Readable := Segment.Flags (Read);
            Writable := Segment.Flags (Write);
            Executable := Segment.Flags (Execute);
            return;
         end if;
      end loop;

      Valid := False;
   end Get_Process_Segment;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Process : Rose.Objects.Process_Id) is
   begin
      Process_Table (Process).State := Available;
   end Kill_Process;

   -----------------
   -- New_Process --
   -----------------

   procedure New_Process
     (Process     : Rose.Objects.Process_Id;
      Resume_Cap  : Rose.Capabilities.Capability;
      Faulted_Cap : Rose.Capabilities.Capability;
      Segments    : Segment_Record_Array)
   is
   begin
      Process_Table (Process) :=
        Memory_Process_Record'
          (State       => Active,
           Resume_Cap  => Resume_Cap,
           Faulted_Cap => Faulted_Cap,
           Segments    => Segments);
   end New_Process;

end Mem.Processes;
