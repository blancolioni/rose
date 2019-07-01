with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Objects;
with Rose.Words;

with Rose.Command_Line;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Block_Device.Server;

with Rose.Server;
with Rose.System_Calls.Client;

package body Partition.Server is

   use Rose.Interfaces.Block_Device;

   Partition_Block_Count : Block_Address_Type;
   Partition_Block_Size  : Block_Size_Type;

   Device_Base_Address   : Block_Address_Type;
   Device_Block_Count    : Block_Address_Type;
   Device_Block_Size     : Block_Size_Type;

   Block_Size_Ratio      : Block_Address_Type;

   Device_Total_Block_Count : Block_Address_Type;

   Device_Client      : Client.Block_Device_Client;
   Partition_Server   : Rose.Server.Server_Context;

   function To_Word_32
     (X : String)
      return Rose.Words.Word_32;

   procedure Get_Parameters
     (Id          : in     Rose.Objects.Capability_Identifier;
      Block_Count :    out Block_Address_Type;
      Block_Size  :    out Rose.Interfaces.Block_Device.Block_Size_Type);

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
   begin
      Console_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Block_Device_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Rose.Console_IO.Open (Console_Cap);

      Client.Open
        (Device_Client, Block_Device_Cap);

      Client.Get_Parameters
        (Device_Client, Device_Total_Block_Count, Device_Block_Size);

      declare
         Argument : String (1 .. 20);
         Last     : Natural;
      begin
         Rose.Command_Line.Get_Argument (1, Argument, Last);
         Device_Base_Address :=
           Block_Address_Type (To_Word_32 (Argument (1 .. Last)));

         Rose.Command_Line.Get_Argument (2, Argument, Last);
         Device_Block_Count :=
           Block_Address_Type (To_Word_32 (Argument (1 .. Last)))
           - Device_Base_Address + 1;

         Rose.Command_Line.Get_Argument (3, Argument, Last);
         Partition_Block_Size :=
           Block_Size_Type (To_Word_32 (Argument (1 .. Last)));
      end;

      Block_Size_Ratio :=
        Block_Address_Type (Partition_Block_Size / Device_Block_Size);

      declare
         use Rose.Words;
      begin
         Partition_Block_Count :=
           Block_Address_Type
             (Word (Device_Block_Count) / Word (Block_Size_Ratio));
      end;

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
         * Natural (Partition_Block_Size)
         / 1024 / 1024);
      Rose.Console_IO.Put ("M");
      Rose.Console_IO.New_Line;

      Rose.Interfaces.Block_Device.Server.Create_Server
        (Server_Context => Partition_Server,
         Get_Parameters => Get_Parameters'Access,
         Read_Blocks    => Read_Blocks'Access,
         Write_Blocks   => Write_Blocks'Access);

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
      Client.Read_Blocks
        (Item   => Device_Client,
         Start  =>
           Device_Base_Address + Start * Block_Size_Ratio,
         Count  => Count * Positive (Block_Size_Ratio),
         Blocks => Blocks);
   end Read_Blocks;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Partition_Server);
   end Start_Server;

   ----------------
   -- To_Word_32 --
   ----------------

   function To_Word_32
     (X : String)
      return Rose.Words.Word_32
   is
      use Rose.Words;
   begin
      return W : Word_32 := 0 do
         for Ch of X loop
            if Ch in '0' .. '9' then
               W := W * 10 + Character'Pos (Ch) - 48;
            end if;
         end loop;
      end return;
   end To_Word_32;

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
      Client.Write_Blocks
        (Item   => Device_Client,
         Start  =>
           Device_Base_Address + Start * Block_Size_Ratio,
         Count  => Count * Positive (Block_Size_Ratio),
         Blocks => Blocks);
   end Write_Blocks;

end Partition.Server;
