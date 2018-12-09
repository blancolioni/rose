with Rose.Console_IO;
with Rose.Words;

with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Mem.Physical_Map;

package body Mem.Calls is

   Log_Calls : constant Boolean := False;

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
         Params.Control.Last_Recv_Cap  := 1;

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
            Unmap_Page_Cap => Params.Caps (1));

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

   ---------
   -- Map --
   ---------

   procedure Map
     (Process    : Rose.Objects.Object_Id;
      Physical   : Rose.Addresses.Physical_Page_Address;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Readable   : Boolean;
      Writeable  : Boolean;
      Executable : Boolean)
   is
      use type Rose.Capabilities.Capability;
      use Rose.Invocation;
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
         Params.Control.Flags := (Send      => True,
                                  Block     => True,
                                  others    => False);
         Params.Cap := Cap;
         Send_Object_Id (Params, Process);
         Send_Word (Params, Word (Physical));
         Send_Word (Params, Word (Virtual));
         Send_Word (Params,
                    Boolean'Pos (Readable)
                    + 2 * Boolean'Pos (Writeable)
                    + 4 * Boolean'Pos (Executable));
         Send_Word (Params, Mem.Physical_Map.Available_Pages);
         Send_Word (Params, Mem.Physical_Map.Allocated_Pages);

         Rose.System_Calls.Invoke_Capability (Params);

         if Params.Control.Flags (Error) then
            Rose.Console_IO.Put_Line
              ("error mapping process page");
         end if;

      end if;
   end Map;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (On_Launch     : Launch_Handler;
      On_Kill       : Kill_Handler;
      On_Page_Fault : Page_Fault_Handler)
   is
      use Rose.Invocation;
      use type Rose.Words.Word_32;
      Launch_Cap     : constant Rose.Capabilities.Capability :=
                         Rose.System_Calls.Server.Create_Endpoint
                           (Create_Endpoint_Cap, Process_Launched_Endpoint);
      Kill_Cap       : constant Rose.Capabilities.Capability :=
                         Rose.System_Calls.Server.Create_Endpoint
                           (Create_Endpoint_Cap, Process_Killed_Endpoint);
      Page_Fault_Cap : constant Rose.Capabilities.Capability :=
                         Rose.System_Calls.Server.Create_Endpoint
                           (Create_Endpoint_Cap, Page_Fault_Endpoint);
      Receive_Cap    : constant Rose.Capabilities.Capability :=
                         Rose.System_Calls.Server.Create_Receive_Cap
                           (Create_Endpoint_Cap);
      Params         : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Control.Flags := (Send => True,
                               Block => True,
                               others    => False);
      Params.Cap := Start_Paging_Cap;
      Send_Cap (Params, Launch_Cap);
      Send_Cap (Params, Kill_Cap);
      Send_Cap (Params, Page_Fault_Cap);
      Rose.System_Calls.Invoke_Capability (Params);

      loop

         Params.Control.Flags :=
           (Receive => True, Block => True, Recv_Words => True,
            others  => False);
         Params.Control.Last_Recv_Word := Parameter_Word_Index'Last;
         Params.Cap := Receive_Cap;

         Rose.System_Calls.Invoke_Capability (Params);

         case Params.Endpoint is
            when Process_Launched_Endpoint =>
               declare
                  Process_Id    : constant Rose.Objects.Object_Id :=
                                    Get_Object_Id (Params, 0);
                  Resume_Cap    : constant Rose.Capabilities.Capability :=
                                    Params.Caps (0);
                  Faulted_Cap   : constant Rose.Capabilities.Capability :=
                                    Params.Caps (1);
                  Segments     : Mem.Processes.Segment_Record_Array;

               begin

                  if Log_Calls then
                     Rose.Console_IO.Put ("mem: process launched: ");
                     Rose.Console_IO.Put (Rose.Words.Word_16 (Process_Id));
                     Rose.Console_IO.Put (" ");
                     Rose.Console_IO.Put ("<");
                     Rose.Console_IO.Put (Rose.Words.Word_8 (Resume_Cap));
                     Rose.Console_IO.Put (" ");
                     Rose.Console_IO.Put (Rose.Words.Word_8 (Faulted_Cap));
                     Rose.Console_IO.Put (">");
                     Rose.Console_IO.New_Line;
                  end if;

                  for I in Segments'Range loop
                     declare
                        use Rose.Addresses;
                        use Rose.Words;
                        use Mem.Processes;
                        Index : constant Parameter_Word_Index :=
                                  Parameter_Word_Index
                                    (I * 3 - 2 +
                                       (if Object_Fits_In_Word
                                        then 0 else 1));
                        Flags : constant Word := Params.Data (Index);
                        Base  : constant Word := Params.Data (Index + 1);
                        Bound : constant Word := Params.Data (Index + 2);
                     begin
                        exit when Bound <= Base;
                        Segments (I) :=
                          Mem.Processes.Segment_Record'
                            (Base  => Virtual_Page_Address (Base),
                             Bound => Virtual_Page_Address (Bound),
                          Flags =>
                               (Read => (Flags and 1) /= 0,
                                Write => (Flags and 2) /= 0,
                                Execute => (Flags and 4) /= 0));
                     end;
                  end loop;

                  On_Launch (Process_Id, Resume_Cap, Faulted_Cap, Segments);

               end;

            when Process_Killed_Endpoint =>
               if Log_Calls then
                  Rose.Console_IO.Put ("mem: process killed: ");
                  Rose.Console_IO.Put (Rose.Words.Word_8 (Params.Data (0)));
                  Rose.Console_IO.New_Line;
               end if;

               On_Kill (Get_Object_Id (Params, 0));

            when Page_Fault_Endpoint =>
               if Log_Calls then
                  Rose.Console_IO.Put ("mem: page fault: ");
                  Rose.Console_IO.Put (Rose.Words.Word_16 (Params.Data (0)));
                  Rose.Console_IO.Put (" ");
                  Rose.Console_IO.Put (Params.Data (1) * 4096);
                  Rose.Console_IO.Put (" ");
                  Rose.Console_IO.Put (Rose.Words.Word_8 (Params.Data (2)));
                  Rose.Console_IO.New_Line;
               end if;

               declare
                  Oid_Index : constant Parameter_Word_Index := 0;
                  Virtual_Page_Index : constant Parameter_Word_Index :=
                                         (if Object_Fits_In_Word
                                          then 1 else 2);
                  Physical_Page_Index : constant Parameter_Word_Index :=
                                          Virtual_Page_Index + 1;
                  Action_Index        : constant Parameter_Word_Index :=
                                          Physical_Page_Index + 1;
               begin
                  On_Page_Fault
                    (Get_Object_Id (Params, Oid_Index),
                     Rose.Addresses.Virtual_Page_Address
                       (Params.Data (Virtual_Page_Index)),
                     Rose.Addresses.Physical_Page_Address
                       (Params.Data (Physical_Page_Index)),
                     Action_Type'Val
                       (Params.Data (Action_Index)));
               end;

            when others =>
               Rose.Console_IO.Put
                 ("mem: no such endpoint: ");
               Rose.Console_IO.Put
                 (Rose.Words.Word_32 (Params.Endpoint));
               Rose.Console_IO.New_Line;
         end case;
      end loop;

   end Receive;

   -----------------
   -- Set_Faulted --
   -----------------

   procedure Set_Faulted
     (Process : Rose.Objects.Object_Id)
   is
   begin
      Rose.System_Calls.Client.Send
        (Cap  => Mem.Processes.Faulted_Capability (Process));
   end Set_Faulted;

   ---------------
   -- Set_Ready --
   ---------------

   procedure Set_Ready
     (Process : Rose.Objects.Object_Id)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Control.Flags := (Send      => True,
                               Block     => True,
                               others    => False);
      Params.Cap := Mem.Processes.Resume_Capability (Process);
      Rose.System_Calls.Invoke_Capability (Params);
   end Set_Ready;

   -----------
   -- Unmap --
   -----------

   procedure Unmap
     (Process    : Rose.Objects.Object_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address)
   is
      use Rose.Words;
   begin
      Rose.System_Calls.Client.Send
        (Cap => Mem.Physical_Map.Region_Unmap_Page_Cap,
         Data => (Word (Process), Word (Virtual)));
   end Unmap;

end Mem.Calls;
