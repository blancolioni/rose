with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Block_Device.Server;
with Rose.Interfaces.Partition_Setup.Server;

with Rose.Server;
with Rose.System_Calls.Client;

package body Partition.Server is

   use Rose.Interfaces.Block_Device;

   Partition_Block_Count : Block_Address_Type := 0;
   Partition_Block_Size  : Block_Size_Type := 1;

   Device_Base_Address   : Block_Address_Type := 0;
   Device_Block_Count    : Block_Address_Type := 0;
   Device_Block_Size     : Block_Size_Type := 1;

   Block_Size_Ratio      : Block_Address_Type := 1;

   Device_Total_Block_Count : Block_Address_Type;

   Device_Client      : Client.Block_Device_Client;
   Partition_Server   : Rose.Server.Server_Context;

   Invocation_Buffer : System.Storage_Elements.Storage_Array
     (1 .. 65536)
     with Alignment => 4096;

   procedure Set_Range
     (Id         : in Rose.Objects.Capability_Identifier;
      Start      : in Rose.Words.Word_64;
      Bound      : in Rose.Words.Word_64;
      Block_Size : in Rose.Words.Word);

   procedure Get_Parameters
     (Id          : in     Rose.Objects.Capability_Identifier;
      Block_Count :    out Block_Address_Type;
      Block_Size  :    out Rose.Interfaces.Block_Device.Block_Size_Type);

   function In_Partition
     (Start : Block_Address_Type;
      Count : Natural)
      return Boolean
   is (Start <= Partition_Block_Count
       and then Block_Address_Type (Count) <= Partition_Block_Count - Start);
   --  True if blocks Start .. Start + Count - 1 are in the partition.  A
   --  request outside it would reach the next partition on the device.

   procedure Report_Out_Of_Range
     (Operation : String;
      Start     : Block_Address_Type;
      Count     : Natural);

   procedure Read_Blocks
     (Id     : in     Rose.Objects.Capability_Identifier;
      Start  : in     Block_Address_Type;
      Count  : in     Natural;
      Blocks :    out System.Storage_Elements.Storage_Array);

   procedure Write_Blocks
     (Id     : in     Rose.Objects.Capability_Identifier;
      Start  : in     Block_Address_Type;
      Count  : in     Natural;
      Blocks : in     System.Storage_Elements.Storage_Array);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is

      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));

      Preload : Natural;
      pragma Import (Ada, Preload);
      for Preload'Address use System'To_Address (16#F000#);

   begin
      Preload := 0;

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap);
      Rose.System_Calls.Use_Buffer
        (Invocation_Buffer'Address, Invocation_Buffer'Length);
      for Element of Invocation_Buffer loop
         Element := 0;
      end loop;

      Console_Cap := Get_Cap (1);
      Block_Device_Cap := Get_Cap (2);

      Rose.Console_IO.Open (Console_Cap);

      Client.Open
        (Device_Client, Block_Device_Cap);

      Client.Get_Parameters
        (Device_Client, Device_Total_Block_Count, Device_Block_Size);

      Rose.Interfaces.Block_Device.Server.Create_Server
        (Server_Context => Partition_Server,
         Get_Parameters => Get_Parameters'Access,
         Read_Blocks    => Read_Blocks'Access,
         Write_Blocks   => Write_Blocks'Access);
      Rose.Interfaces.Partition_Setup.Server.Attach_Interface
        (Partition_Server, Set_Range'Access);
   end Create_Server;

   --------------------
   -- Get_Parameters --
   --------------------

   procedure Get_Parameters
     (Id          : in     Rose.Objects.Capability_Identifier;
      Block_Count :    out Block_Address_Type;
      Block_Size  :    out Rose.Interfaces.Block_Device.Block_Size_Type)
   is
      pragma Unreferenced (Id);
   begin
      Block_Count := Partition_Block_Count;
      Block_Size  := Partition_Block_Size;
   end Get_Parameters;

   -----------------
   -- Read_Blocks --
   -----------------

   procedure Read_Blocks
     (Id     : in     Rose.Objects.Capability_Identifier;
      Start  : in     Block_Address_Type;
      Count  : in     Natural;
      Blocks :    out System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
   begin
      if not In_Partition (Start, Count) then
         Report_Out_Of_Range ("read", Start, Count);
         Blocks := (others => 0);
         return;
      end if;

      Client.Read_Blocks
        (Item   => Device_Client,
         Start  =>
           Device_Base_Address + Start * Block_Size_Ratio,
         Count  => Count * Positive (Block_Size_Ratio),
         Blocks => Blocks);
   end Read_Blocks;

   -------------------------
   -- Report_Out_Of_Range --
   -------------------------

   procedure Report_Out_Of_Range
     (Operation : String;
      Start     : Block_Address_Type;
      Count     : Natural)
   is
   begin
      Rose.Console_IO.Put ("partition: ");
      Rose.Console_IO.Put (Operation);
      Rose.Console_IO.Put (" outside the partition: block ");
      Rose.Console_IO.Put (Rose.Words.Word_64 (Start));
      Rose.Console_IO.Put (" count ");
      Rose.Console_IO.Put (Count);
      Rose.Console_IO.Put (" of ");
      Rose.Console_IO.Put (Rose.Words.Word_64 (Partition_Block_Count));
      Rose.Console_IO.New_Line;
   end Report_Out_Of_Range;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (Id         : in Rose.Objects.Capability_Identifier;
      Start      : in Rose.Words.Word_64;
      Bound      : in Rose.Words.Word_64;
      Block_Size : in Rose.Words.Word)
   is
      use Rose.Words;
      pragma Unreferenced (Id);
   begin
      Device_Base_Address := Block_Address_Type (Start);
      Device_Block_Count :=
        Block_Address_Type (Bound) - Device_Base_Address + 1;
      Partition_Block_Size := Block_Size_Type (Block_Size);

      Block_Size_Ratio :=
        Block_Address_Type (Partition_Block_Size / Device_Block_Size);

      --  The division is of words, which the run-time system can do.

      Partition_Block_Count :=
        Block_Address_Type
          (Word (Device_Block_Count) / Word (Block_Size_Ratio));

      Rose.Console_IO.Put
        ("partition: start ");
      Rose.Console_IO.Put
        ((Natural (Device_Base_Address)
         + Natural (Block_Size_Ratio) - 1)
         / Natural (Block_Size_Ratio));

      Rose.Console_IO.Put ("/");
      Rose.Console_IO.Put
        (Natural (Device_Base_Address));
      Rose.Console_IO.Put
        (" blocks ");
      Rose.Console_IO.Put
        (Natural (Partition_Block_Count));
      Rose.Console_IO.Put ("/");
      Rose.Console_IO.Put
        (Natural (Device_Block_Count));
      Rose.Console_IO.Put
        (" block size ");
      Rose.Console_IO.Put
        (Natural (Partition_Block_Size));
      Rose.Console_IO.Put ("/");
      Rose.Console_IO.Put
        (Natural (Device_Block_Size));

      Rose.Console_IO.Put
        (" total ");
      Rose.Console_IO.Put
        (Natural (Partition_Block_Count)
         / 1024 * Natural (Partition_Block_Size)
         / 1024);
      Rose.Console_IO.Put ("M");
      Rose.Console_IO.New_Line;
   end Set_Range;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Partition_Server);
   end Start_Server;

   ------------------
   -- Write_Blocks --
   ------------------

   procedure Write_Blocks
     (Id     : in     Rose.Objects.Capability_Identifier;
      Start  : in     Block_Address_Type;
      Count  : in     Natural;
      Blocks : in     System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
   begin
      if not In_Partition (Start, Count) then
         Report_Out_Of_Range ("write", Start, Count);
         return;
      end if;

      Client.Write_Blocks
        (Item   => Device_Client,
         Start  =>
           Device_Base_Address + Start * Block_Size_Ratio,
         Count  => Count * Positive (Block_Size_Ratio),
         Blocks => Blocks);
   end Write_Blocks;

end Partition.Server;
