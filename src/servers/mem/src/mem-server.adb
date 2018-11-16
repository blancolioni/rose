with Rose.Addresses;
with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;

with Mem.Calls;
with Mem.Physical_Map;
with Mem.Processes;
with Mem.Virtual_Map;

package body Mem.Server is

   procedure On_Launch
     (Process     : Rose.Objects.Process_Id;
      Resume_Cap  : Rose.Capabilities.Capability;
      Faulted_Cap : Rose.Capabilities.Capability;
      Segments    : Mem.Processes.Segment_Record_Array);

   procedure On_Kill (Process : Rose.Objects.Process_Id);

   procedure On_Page_Fault
     (Process       : Rose.Objects.Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Action        : Action_Type);

   procedure Protection_Fault
     (Process : Rose.Objects.Process_Id;
      Address : Rose.Addresses.Virtual_Page_Address;
      Message : String);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Mem.Calls.Load_Memory_Map;
   end Create_Server;

   -------------
   -- On_Kill --
   -------------

   procedure On_Kill (Process : Rose.Objects.Process_Id) is
   begin
      Rose.Console_IO.Put ("mem: killing process: ");
      Rose.Console_IO.Put (Natural (Process));
      Rose.Console_IO.New_Line;

      Mem.Processes.Kill_Process (Process);
   end On_Kill;

   ---------------
   -- On_Launch --
   ---------------

   procedure On_Launch
     (Process     : Rose.Objects.Process_Id;
      Resume_Cap  : Rose.Capabilities.Capability;
      Faulted_Cap : Rose.Capabilities.Capability;
      Segments    : Mem.Processes.Segment_Record_Array)
   is
   begin
      if False then
         for I in Segments'Range loop
            declare
               use Rose.Words;
               Base : constant Word := Word (Segments (I).Base);
               Bound : constant Word := Word (Segments (I).Bound);
            begin
               if Bound > Base then
                  Rose.Console_IO.Put ("Segment ");
                  Rose.Console_IO.Put (Natural (I));
                  Rose.Console_IO.Put (" ");
                  Rose.Console_IO.Put (Base);
                  Rose.Console_IO.Put (" ");
                  Rose.Console_IO.Put (Bound);
                  Rose.Console_IO.Put (" ");
                  Rose.Console_IO.Put
                    (if Segments (I).Flags (Mem.Processes.Read)
                     then "r" else "-");
                  Rose.Console_IO.Put
                    (if Segments (I).Flags (Mem.Processes.Write)
                     then "w" else "-");
                  Rose.Console_IO.Put
                    (if Segments (I).Flags (Mem.Processes.Execute)
                     then "x" else "-");
                  Rose.Console_IO.New_Line;
               end if;
            end;
         end loop;
      end if;

      Mem.Processes.New_Process
        (Process, Resume_Cap, Faulted_Cap, Segments);
   end On_Launch;

   -------------------
   -- On_Page_Fault --
   -------------------

   procedure On_Page_Fault
     (Process       : Rose.Objects.Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Action        : Action_Type)
   is
      use Rose.Addresses;
      Valid, Readable, Writable, Executable : Boolean := False;
   begin
      if not Mem.Processes.Is_Valid_Process_Id (Process) then
         Protection_Fault (Process, Virtual_Page, "invalid process id");
         return;
      end if;

      Mem.Processes.Get_Process_Segment
        (Process, Virtual_Page, Valid, Readable, Writable, Executable);

      if not Valid then
         Protection_Fault (Process, Virtual_Page, "invalid page reference");
         return;
      end if;

      case Action is
         when Mem.Read =>
            if not Readable then
               Protection_Fault (Process, Virtual_Page,
                                 "attempt to read non-readable page");
               return;
            end if;
         when Mem.Write =>
            if not Writable then
               Protection_Fault (Process, Virtual_Page,
                                 "attempt to write non-writable page");
               return;
            end if;
         when Mem.Execute =>
            if not Executable then
               Protection_Fault (Process, Virtual_Page,
                                 "attempt to execute non-executable page");
               return;
            end if;
      end case;

      if Physical_Page = 0 then
         declare
            Have_Page : Boolean;
            Page      : Rose.Addresses.Physical_Page_Address;
         begin
            Mem.Physical_Map.Allocate_Page
              (Page, Have_Page);
            if not Have_Page then
               Mem.Virtual_Map.Reclaim (Page, Have_Page);
            end if;

            if not Have_Page then
               Mem.Calls.Set_Error (Process);
            else
               Mem.Virtual_Map.Map
                 (Process       => Process,
                  Virtual_Page  => Virtual_Page,
                  Physical_Page => Page,
                  Readable      => True,
                  Writable      => Action = Write,
                  Executable    => Action = Execute);

               Mem.Calls.Set_Ready (Process);
            end if;
         end;
      else
         declare
            Mapping   : constant Mem.Virtual_Map.Virtual_Page_Mapping :=
                          Mem.Virtual_Map.Get (Process, Virtual_Page);
         begin
            case Action is
               when Mem.Read =>
                  Mem.Calls.Set_Error (Process);
               when Mem.Execute =>
                  Mem.Calls.Set_Error (Process);
               when Mem.Write =>
                  if Mem.Virtual_Map.Writable (Mapping) then
                     Mem.Calls.Set_Error (Process);
                  else
                     Mem.Virtual_Map.Set_Read_Write (Mapping);
                  end if;
            end case;
         end;
      end if;

   end On_Page_Fault;

   ----------------------
   -- Protection_Fault --
   ----------------------

   procedure Protection_Fault
     (Process : Rose.Objects.Process_Id;
      Address : Rose.Addresses.Virtual_Page_Address;
      Message : String)
   is
      use Rose.Words;
   begin
      Rose.Console_IO.Put ("mem: error pid ");
      Rose.Console_IO.Put (Natural (Process));
      Rose.Console_IO.Put ("; page ");
      Rose.Console_IO.Put (Word (Address) * 4096);
      Rose.Console_IO.Put (": ");
      Rose.Console_IO.Put (Message);
      Rose.Console_IO.New_Line;
      Mem.Calls.Set_Faulted (Process);
   end Protection_Fault;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Mem.Calls.Receive
        (On_Launch'Access, On_Kill'Access,
         On_Page_Fault'Access);
   end Start_Server;

end Mem.Server;
