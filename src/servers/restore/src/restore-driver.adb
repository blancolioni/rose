with Rose.Devices.Block.Client;
with Rose.Devices.GPT;
with Rose.Devices.Partitions;

with Rose.Console_IO;

with Rose.Words;

procedure Restore.Driver is
   use Rose.Devices.Block.Client;
   Device : Block_Device_Type;
begin
   Rose.Console_IO.Open (Console_Cap);

   Rose.Devices.Block.Client.Open
     (Device         => Device,
      Parameters_Cap => Block_Device_Parameters_Cap,
      Read_Cap       => Block_Device_Read_Cap,
      Write_Cap      => Block_Device_Write_Cap);

   declare
      use Rose.Words;
      Block_Size   : constant Rose.Devices.Block.Block_Size_Type :=
                       Get_Block_Size (Device);
      Block_Count  : constant Rose.Devices.Block.Block_Address_Type :=
                       Get_Block_Count (Device);
      Storage_Size : constant Rose.Devices.Block.Device_Size_Type :=
                       Rose.Devices.Block.To_Device_Size
                         (Block_Size, Block_Count);
   begin
      Rose.Console_IO.Put ("device: size ");
      Rose.Console_IO.Put (Natural (Word_64 (Storage_Size) / 2 ** 20));
      Rose.Console_IO.Put ("M");
      Rose.Console_IO.New_Line;

      if Rose.Devices.GPT.Has_GPT (Device) then
         Rose.Console_IO.Put_Line ("Found GPT partition header");
      else

         Rose.Console_IO.Put_Line ("Intitialising device");

         Rose.Devices.GPT.Initialize_GPT (Device);

         if Rose.Devices.GPT.Has_GPT (Device) then
            declare
               use Rose.Devices.Block;
               use Rose.Devices.Partitions;
               Available_Blocks : constant Block_Address_Type :=
                                    Block_Count - 4;
               Partition_Size   : constant Block_Address_Type :=
                                    Available_Blocks / 4;
               Current_Start    : Block_Address_Type := 3;
               Name             : String  := "rose-swap-#";
            begin

               Rose.Console_IO.Put_Line ("creating partitions");

               for I in 1 .. 4 loop
                  Name (Name'Last) :=
                    Character'Val (48 + I);

                  Rose.Console_IO.Put ("partition: ");
                  Rose.Console_IO.Put (Name);
                  Rose.Console_IO.New_Line;

                  Rose.Devices.GPT.Add_Partition
                    (Block_Device        => Device,
                     First_Block         => Current_Start,
                     Last_Block          => Current_Start + Partition_Size - 1,
                     Partition_Type_Low  => Swap_Id_Low,
                     Partition_Type_High => Swap_Id_High,
                     Partition_Flags     => 0,
                     Partition_Name      => Name);

                  Current_Start := Current_Start + Partition_Size;

               end loop;
            end;

            Rose.Devices.GPT.Write_Partition_Table (Device);
         else
            Rose.Console_IO.Put_Line ("Device initialization failed");
         end if;
      end if;
   end;

end Restore.Driver;
