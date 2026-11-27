with Rose.Console_IO;
with Rose.Limits;
with Rose.Words;

with Rose.Invocation;
with Rose.System_Calls.Client;

with Mem.Page_Table;
with Mem.Physical_Map;
with Mem.Processes;

package body Mem.Calls is

   Log_Calls : constant Boolean := False;

   type Written_Words is array (1 .. 1024) of Rose.Words.Word_32
     with Alignment => 4096;

   Written_Buffer : Written_Words;
   --  The kernel writes the pages written during a checkpoint here, four
   --  words each (End_Page_Copies). It is not on the stack, which is
   --  small.

   ---------------------
   -- Add_Copy_Frames --
   ---------------------

   procedure Add_Copy_Frames (Count : Natural) is
      use type Rose.Capabilities.Capability;
      Params  : aliased Rose.Invocation.Invocation_Record;
      Page    : Rose.Addresses.Physical_Page_Address;
      Success : Boolean := True;
      Left    : Natural := Count;
   begin
      if Add_Copy_Frames_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      --  Eight frames to a call, one in each data word.

      while Left > 0 and then Success loop
         Rose.System_Calls.Initialize_Send (Params, Add_Copy_Frames_Cap);
         for I in 1 .. Natural'Min (Left, 8) loop
            Mem.Physical_Map.Allocate_Page (Page, Success);
            exit when not Success;
            Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Page));
            Left := Left - 1;
         end loop;
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);
      end loop;
   end Add_Copy_Frames;

   ---------------------
   -- Copy_Saved_Page --
   ---------------------

   procedure Copy_Saved_Page
     (Physical : Rose.Addresses.Physical_Page_Address;
      Buffer   : System.Address)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Copy_Saved_Page_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Physical));
      Rose.System_Calls.Send_Buffer
        (Params, Rose.Limits.Page_Size, Buffer, Writable => True);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
   end Copy_Saved_Page;

   ---------------------
   -- End_Page_Copies --
   ---------------------

   procedure End_Page_Copies
     (Saved   : Boolean;
      Written : out Written_Pages;
      Last    : out Natural)
   is
      use type Rose.Capabilities.Capability;
      use type Rose.Objects.Object_Id;
      use type Rose.Words.Word_32;
      Params : aliased Rose.Invocation.Invocation_Record;
      Count  : Natural;
   begin
      Last := 0;

      if End_Page_Copies_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, End_Page_Copies_Cap);
      Rose.System_Calls.Send_Word
        (Params, Rose.Words.Word (Boolean'Pos (Saved)));
      Rose.System_Calls.Send_Buffer
        (Params, Rose.Limits.Page_Size, Written_Buffer'Address,
         Writable => True);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         return;
      end if;

      Count := Natural'Min (Natural (Params.Data (0)), Written'Length);

      for I in 1 .. Count loop
         Written (Written'First + I - 1) :=
           (Process =>
              Rose.Objects.Object_Id (Written_Buffer (4 * I - 3))
            + 2 ** 32 * Rose.Objects.Object_Id (Written_Buffer (4 * I - 2)),
            Page    =>
              Rose.Addresses.Virtual_Page_Address
                (Written_Buffer (4 * I - 1) mod 2 ** 20));
      end loop;

      Last := Written'First + Count - 1;
   end End_Page_Copies;

   ---------------------
   -- Get_Page_Object --
   ---------------------

   function Get_Page_Object
     (Process : Rose.Objects.Object_Id;
      Address : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Page_Object_Id
   is
      use Rose.Objects;
   begin
      return Page_Object_Id'First
        + Process * 2 ** 36
        + Object_Id (Address);
   end Get_Page_Object;

   ----------------------
   -- Get_Process_Page --
   ----------------------

   procedure Get_Process_Page
     (From    : Rose.Objects.Page_Object_Id;
      Process : out Rose.Objects.Object_Id;
      Address : out Rose.Addresses.Virtual_Page_Address)
   is
      use type Rose.Objects.Object_Id;
   begin
      Process := From / 2 ** 36 mod 2 ** 16;
      Address := Rose.Addresses.Virtual_Page_Address (From mod 2 ** 36);
   end Get_Process_Page;

   ---------------------
   -- Has_Page_Copies --
   ---------------------

   function Has_Page_Copies return Boolean is
      use type Rose.Capabilities.Capability;
   begin
      return Copy_Saved_Page_Cap /= Rose.Capabilities.Null_Capability
        and then End_Page_Copies_Cap /= Rose.Capabilities.Null_Capability;
   end Has_Page_Copies;

   -------------------
   -- In_Checkpoint --
   -------------------

   function In_Checkpoint
     (Physical : Rose.Addresses.Physical_Page_Address)
      return Boolean
   is
      use type Rose.Capabilities.Capability;
      use type Rose.Words.Word;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Copy_Saved_Page_Cap = Rose.Capabilities.Null_Capability then
         return True;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Copy_Saved_Page_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Physical));
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      return Params.Control.Flags (Rose.Invocation.Error)
        or else Params.Data (0) /= 0;
   end In_Checkpoint;

   ---------------------
   -- Load_Memory_Map --
   ---------------------

   procedure Load_Memory_Map is
      use Rose.Invocation;
      use Rose.Words;
      Params          : aliased Rose.Invocation.Invocation_Record;
      Total_Available : Word := 0;
      Region_Count    : constant Natural :=
                          Natural
                            (Rose.System_Calls.Client.Get_Value
                               (Region_Count_Cap));
   begin

      for I in 1 .. Region_Count loop

         Params.Cap := Region_Range_Cap;
         Params.Control.Flags := (Send       => True,
                                  Block      => True,
                                  Recv_Words => True,
                                  Recv_Caps  => True,
                                  others     => False);

         Params.Control.Last_Recv_Word := 1;
         Params.Control.Last_Recv_Cap  := 2;

         Send_Word (Params, Word (I));

         Rose.System_Calls.Invoke_Capability (Params);

         Mem.Physical_Map.Add_Region
           (Base           =>
              Rose.Addresses.Physical_Page_Address
                (Params.Data (0)),
            Bound          =>
              Rose.Addresses.Physical_Page_Address
                (Params.Data (1)),
            Map_Page_Cap   => Params.Caps (0),
            Unmap_Page_Cap => Params.Caps (1),
            Set_Page_Cap   => Params.Caps (2));

         if Log_Calls then
            Rose.Console_IO.Put ("region ");
            Rose.Console_IO.Put (I);
            Rose.Console_IO.Put (": ");
            Rose.Console_IO.Put (Params.Data (0) * 4096);
            Rose.Console_IO.Put (" - ");
            Rose.Console_IO.Put (Params.Data (1) * 4096);
            Rose.Console_IO.Put (" <");
            Rose.Console_IO.Put (Rose.Words.Word_8 (Params.Caps (0)));
            Rose.Console_IO.Put (" ");
            Rose.Console_IO.Put (Rose.Words.Word_8 (Params.Caps (1)));
            Rose.Console_IO.Put (">");
            Rose.Console_IO.New_Line;
         end if;

         Total_Available := Total_Available
           + (Params.Data (1) - Params.Data (0));

      end loop;

      Rose.Console_IO.Put ("mem: available memory: ");
      Rose.Console_IO.Put (Natural (Total_Available * 4096 / 1024 / 1024));
      Rose.Console_IO.Put ("M");
      Rose.Console_IO.New_Line;
   end Load_Memory_Map;

   ---------------
   -- Load_Page --
   ---------------

   procedure Load_Page
     (Physical   : Rose.Addresses.Physical_Page_Address;
      Address    : System.Address;
      Fill       : Boolean := False)
   is
      use type Rose.Capabilities.Capability;
      use type Rose.Words.Word;
      Cap    : constant Rose.Capabilities.Capability :=
                 Mem.Physical_Map.Region_Set_Page_Cap (Physical);
      Params : aliased Rose.Invocation.Invocation_Record;
   begin

      if Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put
           ("mem: cannot find map capability for physical page ");
         Rose.Console_IO.Put (Rose.Words.Word (Physical));
         Rose.Console_IO.New_Line;
         return;
      end if;

      --  As for Map, the permissions (read and write) are in the top
      --  three bits of the virtual page's word (#985), and Fill in bit 27
      --  (ADR 0030).

      Rose.System_Calls.Initialize_Send (Params, Cap);
      Rose.System_Calls.Send_Word
        (Params, Rose.Words.Word (Physical));
      Rose.System_Calls.Send_Word
        (Params,
         Rose.Words.Word
           (Rose.Addresses.Virtual_Address_To_Page
                (Rose.Addresses.To_Virtual_Address (Address)))
         + 2 ** 29 * 3
         + (if Fill then 2 ** 27 else 0));

      Rose.System_Calls.Invoke_Capability (Params);
   end Load_Page;

   ---------
   -- Map --
   ---------

   procedure Map
     (Process       : Rose.Objects.Object_Id;
      Physical      : Rose.Addresses.Physical_Page_Address;
      Virtual       : Rose.Addresses.Virtual_Page_Address;
      Readable      : Boolean;
      Writeable     : Boolean;
      Executable    : Boolean;
      Write_Tracked : Boolean := False)
   is
      use type Rose.Capabilities.Capability;
      use Rose.Words;

      Cap : constant Rose.Capabilities.Capability :=
              Mem.Physical_Map.Region_Map_Page_Cap (Physical);
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put
           ("mem: cannot find map capability for physical page ");
         Rose.Console_IO.Put (Rose.Words.Word (Physical));
         Rose.Console_IO.New_Line;
      else
         Rose.System_Calls.Initialize_Send (Params, Cap);
         Rose.System_Calls.Send_Object_Id (Params, Process);
         Rose.System_Calls.Send_Word (Params, Word (Physical));

         --  The permissions are in the top three bits of the virtual
         --  page's word, so that the call fits in eight words (#985), and
         --  Write_Tracked in bit 28 (ADR 0030).

         Rose.System_Calls.Send_Word
           (Params,
            Word (Virtual)
            + 2 ** 29 * Rose.Words.Word'
              (Boolean'Pos (Readable)
               + 2 * Boolean'Pos (Writeable)
               + 4 * Boolean'Pos (Executable))
            + 2 ** 28 * Rose.Words.Word (Boolean'Pos (Write_Tracked)));
         Rose.System_Calls.Send_Word
           (Params, Mem.Physical_Map.Available_Pages);
         Rose.System_Calls.Send_Word
           (Params, Mem.Physical_Map.Allocated_Pages);
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word (Mem.Page_Table.Dirty_Page_Count));
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word (Mem.Processes.Store_Read_Count));

         Rose.System_Calls.Invoke_Capability (Params);

         if Params.Control.Flags (Rose.Invocation.Error) then
            Rose.Console_IO.Put_Line
              ("error mapping process page");
         end if;

      end if;
   end Map;

   -----------------
   -- Unload_Page --
   -----------------

   procedure Unload_Page
     (Physical   : Rose.Addresses.Physical_Page_Address;
      Address    : System.Address)
   is
      use type Rose.Capabilities.Capability;
      Cap    : constant Rose.Capabilities.Capability :=
                 Mem.Physical_Map.Region_Set_Page_Cap (Physical);
      Params : aliased Rose.Invocation.Invocation_Record;
   begin

      if Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put
           ("mem: cannot find map capability for physical page ");
         Rose.Console_IO.Put (Rose.Words.Word (Physical));
         Rose.Console_IO.New_Line;
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Cap);
      Rose.System_Calls.Send_Word
        (Params,
         Rose.Words.Word
           (Rose.Addresses.To_Virtual_Address (Address)));
      Rose.System_Calls.Invoke_Capability (Params);
   end Unload_Page;

   -----------
   -- Unmap --
   -----------

   procedure Unmap
     (Process    : Rose.Objects.Object_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address)
   is
      Unmapped : Boolean;
      pragma Warnings (Off, Unmapped);
   begin
      Unmap (Process, Virtual, Unmapped);
   end Unmap;

   procedure Unmap
     (Process    : Rose.Objects.Object_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Unmapped   : out Boolean)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send
        (Params, Mem.Physical_Map.Region_Unmap_Page_Cap);
      Rose.System_Calls.Send_Object_Id (Params, Process);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Virtual));
      Rose.System_Calls.Invoke_Capability (Params);
      Unmapped := not Params.Control.Flags (Rose.Invocation.Error);
   end Unmap;

end Mem.Calls;
