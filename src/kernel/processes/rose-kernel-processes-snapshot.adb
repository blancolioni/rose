with Rose.Checkpoints;
with Rose.Limits;

package body Rose.Kernel.Processes.Snapshot is

   use System.Storage_Elements;
   use Rose.Words;

   Page_Bytes : constant := Rose.Limits.Page_Size;

   function Is_Saved (P : Kernel_Process_Entry) return Boolean
   is (P.State /= Available and then P.Flags (Persistent));

   function Active_Cap_Count (P : Kernel_Process_Entry) return Natural;

   function Cap_Page_Count (Cap_Count : Natural) return Natural
   is ((Cap_Count + Rose.Checkpoints.Caps_Per_Page - 1)
       / Rose.Checkpoints.Caps_Per_Page);

   procedure Save_Caps
     (P     : Kernel_Process_Entry;
      Pages : System.Address);
   --  Write the active caps of P into the cap pages that start at Pages.

   procedure Save_Process
     (P       : Kernel_Process_Entry;
      Item    : in out Rose.Checkpoints.Process_Record;
      Success : out Boolean);

   ----------------------
   -- Active_Cap_Count --
   ----------------------

   function Active_Cap_Count (P : Kernel_Process_Entry) return Natural is
      Count : Natural := 0;
   begin
      for Cap in 1 .. P.Last_Cap loop
         if P.Is_Active (Cap) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Active_Cap_Count;

   ---------------
   -- Save_Caps --
   ---------------

   procedure Save_Caps
     (P     : Kernel_Process_Entry;
      Pages : System.Address)
   is
      Page_Index : Storage_Offset := 0;
      Cap_Index  : Natural := 0;
   begin
      for Cap in 1 .. P.Last_Cap loop
         if P.Is_Active (Cap) then
            if Cap_Index = Rose.Checkpoints.Caps_Per_Page then
               Page_Index := Page_Index + 1;
               Cap_Index := 0;
            end if;

            Cap_Index := Cap_Index + 1;

            declare
               Page   : Rose.Checkpoints.Cap_Page
                 with Import, Address => Pages + Page_Index * Page_Bytes;
               Layout : Rose.Capabilities.Layout.Capability_Layout;
               Bytes  : Storage_Array
                          (1 .. Rose.Checkpoints.Saved_Layout_Bytes)
                 with Import, Address => Layout'Address;
            begin
               --  Get_Cap reads through the cap cache, which may hold a
               --  newer layout than the cap pages.
               Get_Cap (P.Pid, Cap, Layout);
               Page.Caps (Cap_Index).Index := Word_32 (Cap);
               Page.Caps (Cap_Index).Layout := Bytes;
            end;
         end if;
      end loop;
   end Save_Caps;

   ------------------
   -- Save_Process --
   ------------------

   procedure Save_Process
     (P       : Kernel_Process_Entry;
      Item    : in out Rose.Checkpoints.Process_Record;
      Success : out Boolean)
   is
      Registers : Storage_Array (1 .. P.Stack'Size / 8)
        with Import, Address => P.Stack'Address;
      Current   : Storage_Array (1 .. P.Current_Params'Size / 8)
        with Import, Address => P.Current_Params'Address;
      Queued    : Storage_Array (1 .. P.Queued_Params'Size / 8)
        with Import, Address => P.Queued_Params'Address;
      Flags     : Word_32 := 0;
      Caps      : constant Natural := Active_Cap_Count (P);
      Waiting   : Kernel_Process_Access := P.Waiting_First;
   begin
      Success := True;

      for Flag in Process_Flag loop
         if P.Flags (Flag) then
            Flags := Flags or 2 ** Process_Flag'Pos (Flag);
         end if;
      end loop;

      Item.Magic := Rose.Checkpoints.Process_Record_Magic;
      Item.Oid := Word_64 (P.Oid);
      Item.Name := P.Name;
      Item.State := Process_State'Pos (P.State);
      Item.Flags := Flags;
      Item.Priority := Word_32 (P.Priority);
      Item.Default_Cap := Word_32 (P.Default_Cap);
      Item.Receive_Cap := Word_32 (P.Receive_Cap);
      Item.Send_Cap := Word_32 (P.Send_Cap);
      Item.Last_Cap := Word_32 (P.Last_Cap);
      Item.Cap_Count := Word_32 (Caps);
      Item.Cap_Page_Count := Word_32 (Cap_Page_Count (Caps));
      Item.Command_Endpoint := Word_32 (P.Command_Endpoint);
      Item.Waiting_Endpoint := Word_32 (P.Waiting_Endpoint);
      Item.Waiting_Cap_Id := Word_32 (P.Waiting_Cap_Id);
      Item.Queued_Endpoint := Word_32 (P.Queued_Endpoint);
      Item.Queued_Cap_Id := Word_32 (P.Queued_Cap_Id);
      Item.Checkpoint_Fault := Word_32 (P.Checkpoint_Fault);

      while Waiting /= null loop
         if Item.Waiting_Count = Rose.Checkpoints.Max_Saved_Waiting then
            Success := False;
            return;
         end if;
         Item.Waiting_Count := Item.Waiting_Count + 1;
         Item.Waiting (Natural (Item.Waiting_Count)) :=
           Word_64 (Waiting.Oid);
         Waiting := Waiting.Waiting_Next;
      end loop;

      Item.Registers (1 .. Registers'Length) := Registers;
      Item.Current_Params (1 .. Current'Length) := Current;
      Item.Queued_Params (1 .. Queued'Length) := Queued;

      for I in Page_Range_Index loop
         declare
            Range_Flags : constant Page_Flags := P.Page_Flags (I);
            Saved       : Rose.Checkpoints.Saved_Page_Range renames
                            Item.Page_Ranges (Positive (I));
         begin
            Saved.Base := Word_32 (P.Page_Ranges (I).Base);
            Saved.Bound := Word_32 (P.Page_Ranges (I).Bound);
            Saved.Flags :=
              (if Range_Flags.Valid
               then Rose.Checkpoints.Range_Valid else 0)
              or (if Range_Flags.Readable
                  then Rose.Checkpoints.Range_Readable else 0)
              or (if Range_Flags.Writable
                  then Rose.Checkpoints.Range_Writable else 0)
              or (if Range_Flags.Executable
                  then Rose.Checkpoints.Range_Executable else 0)
              or (if Range_Flags.Disable_Cache
                  then Rose.Checkpoints.Range_Disable_Cache else 0)
              or (if Range_Flags.Write_Through
                  then Rose.Checkpoints.Range_Write_Through else 0)
              or (if Range_Flags.Fixed_Address
                  then Rose.Checkpoints.Range_Fixed_Address else 0);
         end;
      end loop;

      for I in Registered_Endpoint_Index loop
         declare
            Endpoint : Registered_Endpoint_Record renames P.Endpoints (I);
            Saved    : Rose.Checkpoints.Saved_Endpoint renames
                         Item.Endpoints (Positive (I));
         begin
            Saved.Endpoint := Word_64 (Endpoint.Endpoint);
            Saved.Receive_Cap := Word_32 (Endpoint.Receive_Cap);
            Saved.Send_Cap := Word_32 (Endpoint.Send_Cap);
         end;
      end loop;
   end Save_Process;

   ------------------
   -- Write_Tables --
   ------------------

   procedure Write_Tables
     (Storage    : out System.Storage_Elements.Storage_Array;
      Page_Count : out Natural;
      Success    : out Boolean)
   is
      Available     : constant Natural :=
                        Natural (Storage'Length / Page_Bytes);
      Process_Count : Natural := 0;
      Next_Page     : Natural := 1;

      function Page_Address (Index : Positive) return System.Address
      is (Storage (Storage'First
                   + Storage_Offset (Index - 1) * Page_Bytes)'Address);

   begin
      Page_Count := 1;
      for P of Process_Table.all loop
         if Is_Saved (P) then
            Process_Count := Process_Count + 1;
            Page_Count := Page_Count + 1
              + Cap_Page_Count (Active_Cap_Count (P));
         end if;
      end loop;

      if Page_Count > Available then
         Success := False;
         Page_Count := 0;
         return;
      end if;

      Storage (Storage'First
               .. Storage'First + Storage_Offset (Page_Count) * Page_Bytes
                 - 1) := (others => 0);

      declare
         Header : Rose.Checkpoints.Kernel_Table_Header
           with Import, Address => Page_Address (1);
      begin
         Header.Magic := Rose.Checkpoints.Kernel_Table_Magic;
         Header.Version := Rose.Checkpoints.Kernel_Table_Version;
         Header.Page_Count := Word_32 (Page_Count);
         Header.Process_Count := Word_32 (Process_Count);
      end;

      for P of Process_Table.all loop
         if Is_Saved (P) then
            Next_Page := Next_Page + 1;
            declare
               Item : Rose.Checkpoints.Process_Record
                 with Import, Address => Page_Address (Next_Page);
            begin
               Save_Process (P, Item, Success);
               if not Success then
                  Page_Count := 0;
                  return;
               end if;
               if Item.Cap_Page_Count > 0 then
                  Save_Caps (P, Page_Address (Next_Page + 1));
               end if;
               Next_Page := Next_Page + Natural (Item.Cap_Page_Count);
            end;
         end if;
      end loop;

      Success := True;
   end Write_Tables;

end Rose.Kernel.Processes.Snapshot;
