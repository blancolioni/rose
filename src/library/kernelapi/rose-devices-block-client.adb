with Rose.Addresses;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Console_IO;

package body Rose.Devices.Block.Client is

   ---------------------
   -- Get_Block_Count --
   ---------------------

   function Get_Block_Count
     (Device : Block_Device_Type)
      return Block_Address_Type
   is
   begin
      return Device.Block_Count;
   end Get_Block_Count;

   --------------------
   -- Get_Block_Size --
   --------------------

   function Get_Block_Size
     (Device : Block_Device_Type)
      return Block_Size_Type
   is
   begin
      return Device.Block_Size;
   end Get_Block_Size;

   ----------
   -- Open --
   ----------

   procedure Open
     (Device         : in out Block_Device_Type;
      Parameters_Cap : Rose.Capabilities.Capability;
      Read_Cap       : Rose.Capabilities.Capability;
      Write_Cap      : Rose.Capabilities.Capability)
   is
      use Rose.Invocation;
      Params      : aliased Invocation_Record;
   begin
      Device.Open := False;

      Device.Parameters := Parameters_Cap;
      Device.Read := Read_Cap;
      Device.Write := Write_Cap;

      Params.Control.Flags := (Send => True, Rose.Invocation.Block => True,
                               Recv_Words => True, Create_Reply_Cap => True,
                               others     => False);
      Params.Control.Last_Recv_Word := 1;
      Params.Cap := Parameters_Cap;

      Rose.System_Calls.Invoke_Capability (Params);

      Rose.Console_IO.Put ("restore: block size ");
      Rose.Console_IO.Put (Natural (Params.Data (0)));
      Rose.Console_IO.Put ("; block count ");
      Rose.Console_IO.Put (Natural (Params.Data (1)));
      Rose.Console_IO.New_Line;

      Device.Block_Size := Block_Size_Type (Params.Data (0));
      Device.Block_Count := Block_Address_Type (Params.Data (1));

      Device.Open := True;
   end Open;

   ----------------
   -- Read_Block --
   ----------------

   procedure Read_Block
     (Device        : Block_Device_Type;
      Block_Address : Block_Address_Type;
      Buffer        : System.Address)
   is
      Params  : aliased Rose.Invocation.Invocation_Record;
   begin

      if Block_Address >= Device.Block_Count then
         return;
      end if;

      Params.Control.Flags :=
        (Rose.Invocation.Send             => True,
         Rose.Invocation.Block            => True,
         Rose.Invocation.Send_Buffer      => True,
         Rose.Invocation.Writable_Buffer  => True,
         Rose.Invocation.Send_Words       => True,
         Rose.Invocation.Create_Reply_Cap => True,
         others                           => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Cap := Device.Read;
      Params.Data (0) := Rose.Words.Word (Block_Address);
      Params.Buffer_Address :=
        Rose.Addresses.To_Virtual_Address (Buffer);
      Params.Buffer_Length := Rose.Words.Word (Device.Block_Size);

      Rose.System_Calls.Invoke_Capability (Params);

   end Read_Block;

   -----------------
   -- Write_Block --
   -----------------

   procedure Write_Block
     (Device        : Block_Device_Type;
      Block_Address : Block_Address_Type;
      Buffer        : System.Address)
   is
      Params  : aliased Rose.Invocation.Invocation_Record;
   begin

      if Block_Address >= Device.Block_Count then
         return;
      end if;

      Params.Control.Flags :=
        (Rose.Invocation.Send            => True,
         Rose.Invocation.Block           => True,
         Rose.Invocation.Send_Buffer     => True,
         Rose.Invocation.Writable_Buffer => False,
         Rose.Invocation.Send_Words      => True,
         others                          => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Cap := Device.Write;
      Params.Data (0) := Rose.Words.Word (Block_Address);
      Params.Buffer_Address :=
        Rose.Addresses.To_Virtual_Address (Buffer);
      Params.Buffer_Length := Rose.Words.Word (Device.Block_Size);

      Rose.System_Calls.Invoke_Capability (Params);

   end Write_Block;

end Rose.Devices.Block.Client;
