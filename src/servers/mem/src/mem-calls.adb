with Rose.Console_IO;
with Rose.Words;

with Rose.Invocation;
with Rose.System_Calls.Client;

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
        (Params, Rose.Words.Word (Physical));
      Rose.System_Calls.Send_Word
        (Params,
         Rose.Words.Word
           (Rose.Addresses.To_Virtual_Address (Address)));
      Rose.System_Calls.Invoke_Capability (Params);
   end Load_Page;

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
         Rose.System_Calls.Send_Word (Params, Word (Virtual));
         Rose.System_Calls.Send_Word
           (Params,
            Rose.Words.Word'
              (Boolean'Pos (Readable)
               + 2 * Boolean'Pos (Writeable)
               + 4 * Boolean'Pos (Executable)));
         Rose.System_Calls.Send_Word
           (Params, Mem.Physical_Map.Available_Pages);
         Rose.System_Calls.Send_Word
           (Params, Mem.Physical_Map.Allocated_Pages);

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
      use Rose.Words;
   begin
      Rose.System_Calls.Client.Send
        (Cap => Mem.Physical_Map.Region_Unmap_Page_Cap,
         Data => (Word (Process), Word (Virtual)));
   end Unmap;

end Mem.Calls;
