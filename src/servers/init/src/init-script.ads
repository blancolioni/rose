--  generated by Aquarius/Petal
with System.Storage_Elements;

with Init.Commands;                    use Init.Commands;

package Init.Script is

   type Script_Type is
     array (Positive range <>) of Init.Commands.Command_Record;

   Meta_Cap        : constant := 1;
   Launch_Cap      : constant := 2;
   Copy_Cap        : constant := 3;
   Create_Cap      : constant := 4;

   Memory_Buffer       : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   Memory_Buffer_Addr  : constant Init_Register := 16;
   Memory_Cap          : constant Init_Register := 17;
   Console_Process     : constant Init_Register := 18;
   Console_Write       : constant Init_Register := 19;
   Mem_Process         : constant Init_Register := 20;
   Physmem_Cap         : constant Init_Register := 21;
   Start_Paging_Cap    : constant Init_Register := 22;
   PCI_Command_Out     : constant Init_Register := 23;
   PCI_Data_In         : constant Init_Register := 24;
   PCI_Data_Out        : constant Init_Register := 25;
   PCI_Process         : constant Init_Register := 26;
   PCI_Get             : constant Init_Register := 27;
   Ram_Disk_Process    : constant Init_Register := 28;
   Command_0           : constant Init_Register := 29;
   Command_1           : constant Init_Register := 30;
   Control_0           : constant Init_Register := 31;
   Control_1           : constant Init_Register := 32;
   Data_0              : constant Init_Register := 33;
   Data_1              : constant Init_Register := 34;
   ATA_Process         : constant Init_Register := 35;
   PCI_IDE_Cap         : constant Init_Register := 36;
   Storage_Cap         : constant Init_Register := 37;
   Restore_Process     : constant Init_Register := 38;

   S1  : aliased constant String :=
           "init: created console" & Character'Val (10);
   S2  : aliased constant String :=
           "init: created mem" & Character'Val (10);
   S3  : aliased constant String :=
           "init: created PCI driver" & Character'Val (10);
   S4  : aliased constant String :=
           "init: launching ata driver" & Character'Val (10);
   S5  : aliased constant String :=
           "init: created ATA driver" & Character'Val (10);
   S6  : aliased constant String :=
           "init: created ram-disk" & Character'Val (10);
   Init_Script : constant Script_Type := (
      (Move, Memory_Buffer_Addr, 0, (Address_Value, Memory_Buffer'Address)),
      (Move, 101, 0, (Word_Value, 1)),
      (Invoke, Meta_Cap, 3, True, 2, 1, (Memory_Buffer_Addr, 101, others => 0), (Memory_Cap, others => 0)),
      (Move, 102, 0, (Word_Value, 2)),
      (Invoke, Launch_Cap, 1, True, 2, 1, (102, Meta_Cap, others => 0), (Console_Process, others => 0)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Copy_Cap, 1, True, 2, 1, (Console_Process, 100, others => 0), (Console_Write, others => 0)),
      (Move, 200, 0, (String_Value, S1'Access)),
      (Move, 201, 0, (Word_Value, S1'Length)),
      (Copy, Memory_Buffer_Addr, 200, 201),
      (Invoke, Console_Write, 1, False, 2, 0, (Memory_Cap, 201, others => 0), (others => 0)),
      (Invoke, Meta_Cap, 31, False, 0, 0, (others => 0), (others => 0)),
      (Move, 110, 0, (Word_Value, 10)),
      (Move, 106, 0, (Word_Value, 6)),
      (Move, 100, 0, (Word_Value, 0)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Create_Cap, 1, True, 4, 1, (110, 106, 100, 100, others => 0), (Physmem_Cap, others => 0)),
      (Move, 107, 0, (Word_Value, 7)),
      (Move, 102, 0, (Word_Value, 2)),
      (Move, 101, 0, (Word_Value, 1)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Create_Cap, 1, True, 4, 1, (107, 102, 101, 100, others => 0), (Start_Paging_Cap, others => 0)),
      (Move, 103, 0, (Word_Value, 3)),
      (Invoke, Launch_Cap, 1, True, 5, 1, (103, Meta_Cap, Console_Write, Physmem_Cap, Start_Paging_Cap, others => 0), (Mem_Process, others => 0)),
      (Move, 200, 0, (String_Value, S2'Access)),
      (Move, 201, 0, (Word_Value, S2'Length)),
      (Copy, Memory_Buffer_Addr, 200, 201),
      (Invoke, Console_Write, 1, False, 2, 0, (Memory_Cap, 201, others => 0), (others => 0)),
      (Move, 146, 0, (Word_Value, 46)),
      (Move, 114, 0, (Word_Value, 14)),
      (Move, 3420, 0, (Word_Value, 3320)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Create_Cap, 1, True, 4, 1, (146, 114, 3420, 100, others => 0), (PCI_Command_Out, others => 0)),
      (Move, 146, 0, (Word_Value, 46)),
      (Move, 114, 0, (Word_Value, 14)),
      (Move, 3424, 0, (Word_Value, 3324)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Create_Cap, 1, True, 4, 1, (146, 114, 3424, 100, others => 0), (PCI_Data_Out, others => 0)),
      (Move, 130, 0, (Word_Value, 30)),
      (Move, 114, 0, (Word_Value, 14)),
      (Move, 3424, 0, (Word_Value, 3324)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Create_Cap, 1, True, 4, 1, (130, 114, 3424, 100, others => 0), (PCI_Data_In, others => 0)),
      (Move, 104, 0, (Word_Value, 4)),
      (Invoke, Launch_Cap, 1, True, 6, 1, (104, Meta_Cap, Console_Write, PCI_Command_Out, PCI_Data_Out, PCI_Data_In, others => 0), (PCI_Process, others => 0)),
      (Move, 105, 0, (Word_Value, 5)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Copy_Cap, 1, True, 2, 1, (105, 100, others => 0), (PCI_Get, others => 0)),
      (Move, 200, 0, (String_Value, S3'Access)),
      (Move, 201, 0, (Word_Value, S3'Length)),
      (Copy, Memory_Buffer_Addr, 200, 201),
      (Invoke, Console_Write, 1, False, 2, 0, (Memory_Cap, 201, others => 0), (others => 0)),
      (Move, 162, 0, (Word_Value, 62)),
      (Move, 226, 0, (Word_Value, 126)),
      (Move, 596, 0, (Word_Value, 496)),
      (Move, 603, 0, (Word_Value, 503)),
      (Invoke, Create_Cap, 1, True, 4, 1, (162, 226, 596, 603, others => 0), (Command_0, others => 0)),
      (Move, 162, 0, (Word_Value, 62)),
      (Move, 226, 0, (Word_Value, 126)),
      (Move, 468, 0, (Word_Value, 368)),
      (Move, 475, 0, (Word_Value, 375)),
      (Invoke, Create_Cap, 1, True, 4, 1, (162, 226, 468, 475, others => 0), (Command_1, others => 0)),
      (Move, 146, 0, (Word_Value, 46)),
      (Move, 114, 0, (Word_Value, 14)),
      (Move, 1114, 0, (Word_Value, 1014)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Create_Cap, 1, True, 4, 1, (146, 114, 1114, 100, others => 0), (Control_0, others => 0)),
      (Move, 146, 0, (Word_Value, 46)),
      (Move, 114, 0, (Word_Value, 14)),
      (Move, 986, 0, (Word_Value, 886)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Create_Cap, 1, True, 4, 1, (146, 114, 986, 100, others => 0), (Control_1, others => 0)),
      (Move, 130, 0, (Word_Value, 30)),
      (Move, 226, 0, (Word_Value, 126)),
      (Move, 596, 0, (Word_Value, 496)),
      (Move, 603, 0, (Word_Value, 503)),
      (Invoke, Create_Cap, 1, True, 4, 1, (130, 226, 596, 603, others => 0), (Data_0, others => 0)),
      (Move, 130, 0, (Word_Value, 30)),
      (Move, 226, 0, (Word_Value, 126)),
      (Move, 468, 0, (Word_Value, 368)),
      (Move, 475, 0, (Word_Value, 375)),
      (Invoke, Create_Cap, 1, True, 4, 1, (130, 226, 468, 475, others => 0), (Data_1, others => 0)),
      (Move, 200, 0, (String_Value, S4'Access)),
      (Move, 201, 0, (Word_Value, S4'Length)),
      (Copy, Memory_Buffer_Addr, 200, 201),
      (Invoke, Console_Write, 1, False, 2, 0, (Memory_Cap, 201, others => 0), (others => 0)),
      (Move, 106, 0, (Word_Value, 6)),
      (Invoke, Launch_Cap, 1, True, 10, 1, (106, Meta_Cap, Console_Write, PCI_Get, Command_0, Control_0, Data_0, Command_1, Control_1, Data_1, others => 0), (ATA_Process, others => 0)),
      (Move, 200, 0, (String_Value, S5'Access)),
      (Move, 201, 0, (Word_Value, S5'Length)),
      (Copy, Memory_Buffer_Addr, 200, 201),
      (Invoke, Console_Write, 1, False, 2, 0, (Memory_Cap, 201, others => 0), (others => 0)),
      (Move, 100, 0, (Word_Value, 0)),
      (Invoke, Copy_Cap, 1, True, 2, 1, (ATA_Process, 100, others => 0), (Storage_Cap, others => 0)),
      (Move, 107, 0, (Word_Value, 7)),
      (Invoke, Launch_Cap, 1, True, 4, 1, (107, Meta_Cap, Console_Write, Storage_Cap, others => 0), (Restore_Process, others => 0)),
      (Invoke, Meta_Cap, 31, False, 0, 0, (others => 0), (others => 0)),
      (Move, 105, 0, (Word_Value, 5)),
      (Invoke, Launch_Cap, 1, True, 3, 1, (105, Meta_Cap, Console_Write, others => 0), (Ram_Disk_Process, others => 0)),
      (Move, 200, 0, (String_Value, S6'Access)),
      (Move, 201, 0, (Word_Value, S6'Length)),
      (Copy, Memory_Buffer_Addr, 200, 201),
      (Invoke, Console_Write, 1, False, 2, 0, (Memory_Cap, 201, others => 0), (others => 0))
     );

end Init.Script;
