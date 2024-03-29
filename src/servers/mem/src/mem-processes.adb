with Rose.Console_IO;
with Rose.System_Calls.Server;
with Rose.Limits;

with Rose.Interfaces.Heap;
with Rose.Interfaces.Process;
with Rose.Interfaces.Kernel_Process.Client;

with Mem.Calls;

with Mem.Physical_Map;
with Mem.Virtual_Map;

package body Mem.Processes is

   Log_Segments : constant Boolean := False;

   Environment_Base              : constant := 16#E000_0000#;
   Environment_Bound             : constant := 16#E100_0000#;

   type Process_State is (Available, Active, Faulted);

   type Segment_Flag is (Read, Write, Execute, Extensible, Persistent);

   type Segment_Flag_Array is array (Segment_Flag) of Boolean;

   subtype Page_Buffer is
     System.Storage_Elements.Storage_Array (1 .. 4096);
   Long_Buffer : System.Storage_Elements.Storage_Array (1 .. 8192);

   Max_Segments : constant := 8;
   type Segment_Count is range 0 .. Max_Segments;
   subtype Segment_Index is Segment_Count range 1 .. Segment_Count'Last;

   type Segment_Record is
      record
         Base          : Rose.Addresses.Virtual_Page_Address := 0;
         Bound         : Rose.Addresses.Virtual_Page_Address := 0;
         Limit         : Rose.Addresses.Virtual_Page_Address := 0;
         Region        : Rose.Interfaces.Region.Client.Region_Client;
         Region_Base   : Rose.Objects.Object_Id := 0;
         Region_Bound  : Rose.Objects.Object_Id := 0;
         Region_Offset : Rose.Objects.Object_Id := 0;
         Flags         : Segment_Flag_Array := (others => False);
      end record;

   type Segment_Record_Array is array (Segment_Index) of Segment_Record;

   subtype Kernel_Process_Client is
     Rose.Interfaces.Kernel_Process.Client.Kernel_Process_Client;

   type Memory_Process_Record is
      record
         State        : Process_State := Available;
         Oid          : Rose.Objects.Object_Id;
         Segments     : Segment_Record_Array;
         Num_Segments : Segment_Count;
         Process      : Kernel_Process_Client;
         Heap         : Rose.Capabilities.Capability;
         Server       : Rose.Capabilities.Capability;
      end record;

   subtype Real_Process_Id is
     Process_Id range 1 .. Rose.Limits.Max_Processes;

   type Memory_Process_Table is
     array (Real_Process_Id) of aliased Memory_Process_Record;

   Process_Table : Memory_Process_Table;
   Next_Pid      : Real_Process_Id := 1;

   procedure Report_Segment (Segment : Segment_Record);

   ---------------------
   -- Add_Environment --
   ---------------------

   procedure Add_Environment
     (Process       : Process_Id;
      Environment   : System.Storage_Elements.Storage_Array)
   is
      use System.Storage_Elements;
      use Rose.Addresses;

      Virt_Page : Rose.Addresses.Virtual_Page_Address :=
                    Rose.Addresses.Virtual_Address_To_Page
                      (Environment_Base);

      procedure Save_Environment_Page
        (Index : in out Storage_Offset);

      ---------------------------
      -- Save_Environment_Page --
      ---------------------------

      procedure Save_Environment_Page
        (Index : in out Storage_Offset)
      is
         Have_Page : Boolean;
         Phys_Page : Physical_Page_Address;
         Base      : constant Storage_Offset := Index;
         Bound     : constant Storage_Offset :=
                       Index + Physical_Page_Bytes;
      begin

         Mem.Physical_Map.Allocate_Page
           (Phys_Page, Have_Page);

         if not Have_Page then
            Mem.Virtual_Map.Reclaim (Phys_Page, Have_Page);
         end if;

         if not Have_Page then
            Rose.Console_IO.Put_Line ("mem: environment: no page");
         else

            declare
               Addr      : constant System.Address :=
                             To_System_Address
                               (Rose.Addresses.Virtual_Page_To_Address
                                  (Virt_Page));
               Saved_Env : Page_Buffer;
               pragma Import (Ada, Saved_Env);
               for Saved_Env'Address use Addr;
            begin
               Mem.Calls.Load_Page (Phys_Page, Addr);
               Saved_Env := Environment (Base .. Bound - 1);
               Mem.Calls.Unload_Page (Phys_Page, Addr);
            end;

            Mem.Virtual_Map.Map
              (Process       => Process_Table (Process).Oid,
               Virtual_Page  => Virt_Page,
               Physical_Page => Phys_Page,
               Readable      => True,
               Writable      => True,
               Executable    => False,
               Persistent    => True);

            Index := Bound;
            Virt_Page := Virt_Page + 1;
         end if;
      end Save_Environment_Page;

      Page_Bound  : constant Virtual_Page_Address :=
                      Virtual_Address_To_Page
                        (Environment_Bound);
      Start_Index : Storage_Offset := Environment'First;

   begin
      while Start_Index <= Environment'Last
        and then Virt_Page < Page_Bound
      loop
         Save_Environment_Page (Start_Index);
      end loop;
   end Add_Environment;

   -------------------------------
   -- Add_Nonpersistent_Segment --
   -------------------------------

   procedure Add_Nonpersistent_Segment
     (Process        : Process_Id;
      Virtual_Base   : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound  : Rose.Addresses.Virtual_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      Resizable      : Boolean)
   is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      if P.Num_Segments = Max_Segments then
         return;
      end if;

      P.Num_Segments := P.Num_Segments + 1;
      P.Segments (P.Num_Segments) := Segment_Record'
        (Base         => Virtual_Base,
         Bound        => (if Resizable then Virtual_Base else Virtual_Bound),
         Limit        => Virtual_Bound,
         Region       => <>,
         Region_Base  => 0,
         Region_Bound => 0,
         Region_Offset => 0,
         Flags        =>
           (Read       => Readable,
            Write      => Writable,
            Execute    => Executable,
            Extensible => Resizable,
            Persistent => False));

   end Add_Nonpersistent_Segment;

   -----------------
   -- Add_Segment --
   -----------------

   procedure Add_Segment
     (Process       : Process_Id;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound : Rose.Addresses.Virtual_Page_Address;
      Region        : Rose.Interfaces.Region.Client.Region_Client;
      Region_Offset : Rose.Words.Word;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Resizable     : Boolean)
   is
      use type Rose.Objects.Object_Id;
      P : Memory_Process_Record renames Process_Table (Process);
      Region_Base : Rose.Objects.Page_Object_Id;
      Region_Bound : Rose.Objects.Page_Object_Id;
   begin
      if P.Num_Segments = Max_Segments then
         return;
      end if;

      Rose.Interfaces.Region.Client.Get_Range
        (Region, Region_Base, Region_Bound);

      P.Num_Segments := P.Num_Segments + 1;
      P.Segments (P.Num_Segments) := Segment_Record'
        (Base         => Virtual_Base,
         Bound        => (if Resizable then Virtual_Base else Virtual_Bound),
         Limit        => Virtual_Bound,
         Region       => Region,
         Region_Base  => Region_Base,
         Region_Bound => Region_Bound,
         Region_Offset =>
           Rose.Objects.Object_Id (Region_Offset) / Rose.Limits.Page_Size,
         Flags        =>
           (Read => Readable,
            Write => Writable,
            Execute => Executable,
            Extensible => Resizable,
            Persistent => True));

      if Log_Segments then
         Report_Segment (P.Segments (P.Num_Segments));
      end if;

   end Add_Segment;

   ----------------------------
   -- Allocate_Physical_Page --
   ----------------------------

   procedure Allocate_Physical_Page
     (Process       : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      R, W, X, P    : Boolean := False)
   is
      Have_Page : Boolean;
      Page      : Rose.Addresses.Physical_Page_Address;
      Rec       : Memory_Process_Record renames Process_Table (Process);
   begin
      Mem.Physical_Map.Allocate_Page
        (Page, Have_Page);
      if not Have_Page then
         Mem.Virtual_Map.Reclaim (Page, Have_Page);
      end if;

      if not Have_Page then
         Rose.Console_IO.Put_Line ("no page");
         return;
      else

         Initialize_Page
           (Process       => Process,
            Physical_Page => Page,
            Virtual_Page  => Virtual_Page);

         Mem.Virtual_Map.Map
           (Process       => Rec.Oid,
            Virtual_Page  => Virtual_Page,
            Physical_Page => Page,
            Readable      => R,
            Writable      => W,
            Executable    => X,
            Persistent    => P);

         declare
            use Rose.Words;
         begin
            Rose.Console_IO.Put ("mem: preallocating phys ");
            Rose.Console_IO.Put (Rose.Words.Word_32 (Virtual_Page) * 4096);
            Rose.Console_IO.Put (" -> ");
            Rose.Console_IO.Put (Rose.Words.Word_32 (Page) * 4096);
            Rose.Console_IO.New_Line;
         end;
      end if;
   end Allocate_Physical_Page;

   -------------------
   -- Fault_Process --
   -------------------

   procedure Fault_Process
     (Process : Process_Id;
      Message : String)
   is
      use Rose.Interfaces.Kernel_Process.Client;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Rose.Console_IO.Put_Line (Message);
      Fault (P.Process);
      P.State := Faulted;
   end Fault_Process;

   ------------------
   -- Get_Heap_Cap --
   ------------------

   function Get_Heap_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability
   is
   begin
      return Process_Table (Process).Heap;
   end Get_Heap_Cap;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id
     (Process : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id
   is
      Pid : constant Process_Id := Process_Id (Process);
   begin
      if Pid not in Real_Process_Id
        or else Process_Table (Pid).State = Available
      then
         return Rose.Objects.Null_Object_Id;
      else
         return Process_Table (Pid).Oid;
      end if;
   end Get_Object_Id;

   ------------------------
   -- Get_Page_Object_Id --
   ------------------------

   function Get_Page_Object_Id
     (Process         : Process_Id;
      Virtual_Page    : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Object_Id
   is
      use Rose.Addresses;
      use type Rose.Objects.Object_Id;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Virtual_Page >= Segment.Base
           and then Virtual_Page < Segment.Bound
           and then Segment.Region_Base /= 0
         then
            return Segment.Region_Base
              + Rose.Objects.Object_Id (Virtual_Page - Segment.Base);
         end if;
      end loop;

      return 0;
   end Get_Page_Object_Id;

   ----------------------------
   -- Get_Process_Heap_Bound --
   ----------------------------

   function Get_Process_Heap_Bound
     (Process : Process_Id)
     return Rose.Addresses.Virtual_Page_Address
   is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Segment.Flags (Extensible) then
            return Segment.Bound;
         end if;
      end loop;
      return 0;
   end Get_Process_Heap_Bound;

   --------------------
   -- Get_Process_Id --
   --------------------

   function Get_Process_Id
     (Process : Rose.Objects.Object_Id)
      return Process_Id
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
     (Process         : Process_Id;
      Virtual_Page    : Rose.Addresses.Virtual_Page_Address;
      Page_Object     : out Rose.Objects.Object_Id;
      Valid           : out Boolean;
      Readable        : out Boolean;
      Writable        : out Boolean;
      Executable      : out Boolean;
      Persistent      : out Boolean)
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
            Persistent := Segment.Flags (Mem.Processes.Persistent);
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

   -------------------------
   -- Get_Virtual_Address --
   -------------------------

   function Get_Virtual_Address
     (Process : Process_Id;
      Page    : Rose.Objects.Page_Object_Id)
      return Rose.Addresses.Virtual_Page_Address
   is
      use Rose.Objects;
      use type Rose.Addresses.Virtual_Page_Address;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Page >= Segment.Region_Base
           and then Page < Segment.Region_Bound
         then
            return Segment.Base
              + Rose.Addresses.Virtual_Page_Address
              (Page - Segment.Region_Base);
         end if;
      end loop;

      return 0;
   end Get_Virtual_Address;

   ---------------------
   -- Initialize_Page --
   ---------------------

   procedure Initialize_Page
     (Process       : Process_Id;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address)
   is
      use Rose.Addresses;
      P : Memory_Process_Record renames Process_Table (Process);
      Base : constant System.Address := Long_Buffer'Address;
      X    : constant Virtual_Address := To_Virtual_Address (Base);
      Y    : constant Virtual_Address :=
               (if X mod 4096 = 0 then X
                else X - (X mod 4096) + 4096);
      Addr : constant System.Address := To_System_Address (Y);
      Buffer : Page_Buffer;
      pragma Import (Ada, Buffer);
      for Buffer'Address use Addr;
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
                             + Object_Id (Virtual_Page - Segment.Base)
                             + Segment.Region_Offset;
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
     (Process : Process_Id)
      return Boolean
   is
   begin
      return Process in Real_Process_Id
        and then Process_Table (Process).State = Active;
   end Is_Valid_Process_Id;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Process : Process_Id) is
      use Rose.Interfaces.Kernel_Process.Client;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Destroy (P.Process);
      P.State := Available;
   end Kill_Process;

   -----------------
   -- New_Process --
   -----------------

   function New_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
   begin
      return Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap   => Create_Endpoint_Cap,
         Endpoint_Id  =>
           Rose.Interfaces.Process.Process_Interface,
         Identifier   =>
           Rose.Objects.Capability_Identifier
             (Register_Process (Process_Cap)));
   end New_Process;

   -----------------------------
   -- Published_Interface_Cap --
   -----------------------------

   function Published_Interface_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability
   is
   begin
      return Process_Table (Process).Server;
   end Published_Interface_Cap;

   ----------------------
   -- Register_Process --
   ----------------------

   function Register_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Process_Id
   is
      Start  : constant Process_Id := Next_Pid;
      Client : Rose.Interfaces.Kernel_Process.Client.Kernel_Process_Client;
   begin
      while Process_Table (Next_Pid).State /= Available loop
         if Next_Pid = Process_Table'Last then
            Next_Pid := Process_Table'First;
         else
            Next_Pid := Next_Pid + 1;
         end if;
         if Next_Pid = Start then
            Rose.Console_IO.Put_Line ("out of processes");
            return 0;
         end if;
      end loop;

      Rose.Interfaces.Kernel_Process.Client.Open (Client, Process_Cap);

      declare
         Oid : constant Rose.Objects.Object_Id :=
                 Rose.Interfaces.Kernel_Process.Client.Get_Object_Id (Client);
         Heap_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Endpoint
                        (Create_Cap   => Create_Endpoint_Cap,
                         Endpoint_Id  =>
                           Rose.Interfaces.Heap.Heap_Interface,
                         Identifier   =>
                           Rose.Objects.Capability_Identifier (Next_Pid));
      begin
         Process_Table (Next_Pid) :=
           Memory_Process_Record'
             (State        => Active,
              Oid          => Oid,
              Segments     => <>,
              Num_Segments => 0,
              Heap         => Heap_Cap,
              Server       => 0,
              Process      => Client);
      end;

      return Next_Pid;
   end Register_Process;

   --------------------
   -- Report_Segment --
   --------------------

   procedure Report_Segment (Segment : Segment_Record) is

      use type Rose.Words.Word;
      use type Rose.Objects.Object_Id;

      procedure Put (Flag : Segment_Flag;
                     Char : Character);

      ---------
      -- Put --
      ---------

      procedure Put (Flag : Segment_Flag;
                     Char : Character)
      is
      begin
         if Segment.Flags (Flag) then
            Rose.Console_IO.Put (Char);
         else
            Rose.Console_IO.Put ('-');
         end if;
      end Put;

      Base : constant Rose.Words.Word :=
               Rose.Words.Word (Segment.Region_Base + Segment.Region_Offset)
               * 4096;
      Bound : constant Rose.Words.Word :=
                Base + (Rose.Words.Word (Segment.Bound)
                        - Rose.Words.Word (Segment.Base)) * 4096;

   begin
      Rose.Console_IO.Put ("mem: region ");
      Rose.Console_IO.Put (Base);
      Rose.Console_IO.Put (" - ");
      Rose.Console_IO.Put (Bound);

      Rose.Console_IO.Put ("; flags ");
      Put (Read, 'r');
      Put (Write, 'w');
      Put (Execute, 'x');
      Put (Extensible, 's');
      Put (Persistent, 'p');
      Rose.Console_IO.New_Line;
   end Report_Segment;

   --------------------
   -- Resize_Segment --
   --------------------

   procedure Resize_Segment
     (Process           : Process_Id;
      New_Virtual_Bound : Rose.Addresses.Virtual_Page_Address)
   is
   begin

      if Is_Valid_Process_Id (Process) then
         declare
            use Rose.Addresses;
            P : Memory_Process_Record renames Process_Table (Process);
         begin
            for Segment of P.Segments loop
               if New_Virtual_Bound in Segment.Base .. Segment.Limit then
                  if New_Virtual_Bound < Segment.Bound then
                     for I in New_Virtual_Bound .. Segment.Bound - 1 loop
                        Mem.Calls.Unmap (P.Oid, I);
                     end loop;
                  end if;

                  Segment.Bound := New_Virtual_Bound;
                  return;
               end if;
            end loop;
         end;
      end if;
   end Resize_Segment;

   --------------------
   -- Resume_Process --
   --------------------

   procedure Resume_Process
     (Process         : Process_Id)
   is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Rose.Interfaces.Kernel_Process.Client.Resume (P.Process);
   end Resume_Process;

   ---------------------------------
   -- Set_Published_Interface_Cap --
   ---------------------------------

   procedure Set_Published_Interface_Cap
     (Process : Process_Id;
      Cap     : Rose.Capabilities.Capability)
   is
   begin
      Process_Table (Process).Server := Cap;
   end Set_Published_Interface_Cap;

end Mem.Processes;
