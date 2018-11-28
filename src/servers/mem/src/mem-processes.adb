with Rose.Console_IO;
with Rose.Words;

package body Mem.Processes is

   -------------------------
   -- Get_Process_Segment --
   -------------------------

   procedure Get_Process_Segment
     (Process         : Rose.Objects.Object_Id;
      Virtual_Address : Rose.Addresses.Virtual_Page_Address;
      Valid           : out Boolean;
      Readable        : out Boolean;
      Writable        : out Boolean;
      Executable      : out Boolean)
   is
      use Rose.Addresses;
      Pid : constant Process_Id := To_Process_Id (Process);
      P : Memory_Process_Record renames Process_Table (Pid);
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

   -------------------------
   -- Is_Valid_Process_Id --
   -------------------------

   function Is_Valid_Process_Id
     (Process : Rose.Objects.Object_Id)
      return Boolean
   is
      Id : constant Process_Id := To_Process_Id (Process);
   begin
      return Id in Real_Process_Id
        and then Process_Table (Id).State = Active;
   end Is_Valid_Process_Id;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Process : Rose.Objects.Object_Id) is
   begin
      Process_Table (To_Process_Id (Process)).State := Available;
   end Kill_Process;

   -----------------
   -- New_Process --
   -----------------

   procedure New_Process
     (Process     : Rose.Objects.Object_Id;
      Resume_Cap  : Rose.Capabilities.Capability;
      Faulted_Cap : Rose.Capabilities.Capability;
      Segments    : Segment_Record_Array)
   is
      Start : constant Process_Id := Next_Pid;
   begin
      while Process_Table (Next_Pid).State /= Available loop
         if Next_Pid = Process_Table'Last then
            Next_Pid := Process_Table'First;
         else
            Next_Pid := Next_Pid + 1;
         end if;
         if Next_Pid = Start then
            Rose.Console_IO.Put_Line ("out of processes");
            return;
         end if;
      end loop;

      Process_Table (Next_Pid) :=
        Memory_Process_Record'
          (Oid         => Process,
           State       => Active,
           Resume_Cap  => Resume_Cap,
           Faulted_Cap => Faulted_Cap,
           Segments    => Segments);
   end New_Process;

   -------------------
   -- To_Process_Id --
   -------------------

   function To_Process_Id
     (Object : Rose.Objects.Object_Id)
      return Process_Id
   is
      use Rose.Words;
      W_Obj : constant Word_32 :=
                Word_32 (Object);
   begin
      for I in Process_Table'Range loop
         --  work-around for broken 64-bit compare in gcc 8.2.1
         declare
            X : constant Word_32 := Word_32 (Process_Table (I).Oid);
         begin
            if X = W_Obj then
               return I;
            end if;
         end;
      end loop;
      return 0;
   end To_Process_Id;

end Mem.Processes;
