with Ada.Streams;
with Ada.Text_IO;
with System.Storage_Elements;

with Rose.Invocation;
with Rose.Server;
with Rose.Standard_Streams;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Devices.Interrupt_Handler.Server;
with Rose.Interfaces.Devices.Network_Device.Server;
with Rose.Interfaces.Devices.Pci_Device.Client;
with Rose.Interfaces.Processes.Restorable.Server;

with Virtio_Net.Device;

package body Virtio_Net.Server is

   use Ada.Streams;
   use type Rose.Capabilities.Capability;

   --  One object is the card's Network_Device, which is Restorable, so
   --  that the card is started again after a restore. Another takes the
   --  card's interrupt.

   type Card_Object is limited new
     Rose.Interfaces.Devices.Network_Device.Server.Network_Device_Interface
     and Rose.Interfaces.Processes.Restorable.Server.Restorable_Interface
     with null record;

   overriding function Network_Device_Cap
     (Item : in out Card_Object)
      return Rose.Capabilities.Capability;

   overriding function Restorable_Cap
     (Item : in out Card_Object)
      return Rose.Capabilities.Capability;

   overriding function Mac_Address
     (Item : in out Card_Object)
      return Rose.Words.Word_64;

   overriding function Link_Up
     (Item : in out Card_Object)
      return Boolean;

   overriding procedure Notify
     (Item   : in out Card_Object;
      Signal : Rose.Capabilities.Capability);

   overriding procedure Send_Frame
     (Item  : in out Card_Object;
      Frame : Ada.Streams.Stream_Element_Array);

   overriding procedure Receive_Frame
     (Item  : in out Card_Object;
      Frame : out Ada.Streams.Stream_Element_Array;
      Last  : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Restored (Item : in out Card_Object);

   type Interrupt_Object is
     new Rose.Interfaces.Devices.Interrupt_Handler.Server
       .Interrupt_Handler_Interface
     with null record;

   overriding function Interrupt_Handler_Cap
     (Item : in out Interrupt_Object)
      return Rose.Capabilities.Capability;

   overriding procedure Handle_Interrupt
     (Item : in out Interrupt_Object;
      Code : Rose.Words.Word);

   Frame_Not_Sent : exception;
   --  Send_Frame fails with this, which the stack treats as a frame lost
   --  on the wire

   Context       : Rose.Server.Server_Context;
   The_Card      : aliased Card_Object;
   The_Interrupt : aliased Interrupt_Object;

   Has_Card : Boolean := False;
   --  The card is there, and started

   Ack_Cap : Rose.Capabilities.Capability :=
               Rose.Capabilities.Null_Capability;
   --  The kernel masks the card's line, which is level-triggered, when
   --  it sends the interrupt, and this cap unmasks it (#295)

   Signal_Cap : Rose.Capabilities.Capability :=
                  Rose.Capabilities.Null_Capability;
   --  The client's signal, from Notify

   --  The test of a test install (self-tests on the kernel's command
   --  line): an ARP request for QEMU's gateway, and another once the
   --  interrupt of the first reply has been acknowledged, so that the
   --  second reply shows that the line was unmasked.

   type Test_State is (Off, Waiting_First, Waiting_Second, Done);
   Test        : Test_State := Off;
   Send_Second : Boolean := False;

   procedure Acknowledge;
   --  Unmask the card's line. It does no harm when the line isn't
   --  masked, so Send_Frame and Receive_Frame do it too: if the kernel
   --  ever lost an interrupt message, the line would otherwise stay
   --  masked.

   procedure Enable_PCI;
   --  Switch on the card's I/O space and bus mastering

   function Has_Argument
     (Command_Line : Rose.Capabilities.Capability;
      Name         : String)
      return Boolean;
   --  True if Name is a word of the kernel's command line

   function Open_Console return Boolean;
   --  Make the console standard output. False if there is no console
   --  cap, which means that there is no card (Virtio_Net.Console_Writer_Cap)

   procedure Put_Line (Text : String);
   --  Write Text and a new line to the console, and flush it: Text_IO
   --  keeps what it writes until a flush, and the driver never ends

   procedure Raise_Signal;
   --  Tell the client that a frame has arrived

   procedure Register_IRQ;
   --  Register The_Interrupt for the card's interrupt, and keep the cap
   --  that acknowledges it

   procedure Run_Test;
   --  Take the frames waiting, and look for the replies of the test

   procedure Send_ARP_Request;
   --  Ask who has 10.0.2.2, QEMU's gateway, from 10.0.2.15

   -----------------
   -- Acknowledge --
   -----------------

   procedure Acknowledge is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Ack_Cap /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Initialize_Send (Params, Ack_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end if;
   end Acknowledge;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      Started : Boolean;
   begin
      Rose.Interfaces.Processes.Restorable.Server.Attach
        (Context, The_Card'Access);

      if not Open_Console then
         return;
      end if;

      Virtio_Net.Device.Map_Pages;
      Enable_PCI;
      Virtio_Net.Device.Start (Started);

      if not Started then
         Put_Line ("virtio-net: the card did not start");
         return;
      end if;

      Has_Card := True;

      Rose.Interfaces.Devices.Interrupt_Handler.Server.Attach
        (Context, The_Interrupt'Access);
      Register_IRQ;
      Rose.Interfaces.Devices.Network_Device.Server.Publish
        (Context, The_Card'Access);

      if Has_Argument (Command_Line_Cap, "self-tests") then
         Put_Line ("virtio-net test: card started: ok");
         Test := Waiting_First;
         Send_ARP_Request;
      end if;
   end Create_Server;

   ----------------
   -- Enable_PCI --
   ----------------

   procedure Enable_PCI is
      PCI : Rose.Interfaces.Devices.Pci_Device.Client.Pci_Device_Client;
   begin
      Rose.Interfaces.Devices.Pci_Device.Client.Open (PCI, PCI_Device_Cap);
      PCI.Set_Command (16#0005#);
   end Enable_PCI;

   ----------------------
   -- Handle_Interrupt --
   ----------------------

   overriding procedure Handle_Interrupt
     (Item : in out Interrupt_Object;
      Code : Rose.Words.Word)
   is
      pragma Unreferenced (Item, Code);
   begin

      --  Reading the ISR status makes the card drop its line, so the
      --  line can be unmasked once the frames have been seen to.

      Virtio_Net.Device.Interrupted;

      if Test in Waiting_First | Waiting_Second then
         Run_Test;
      elsif Virtio_Net.Device.Frame_Waiting then
         Raise_Signal;
      end if;

      Acknowledge;

      if Send_Second then
         Send_Second := False;
         Send_ARP_Request;
      end if;
   end Handle_Interrupt;

   ------------------
   -- Has_Argument --
   ------------------

   function Has_Argument
     (Command_Line : Rose.Capabilities.Capability;
      Name         : String)
      return Boolean
   is
      use System.Storage_Elements;
      use type Rose.Words.Word;

      Header_Size : constant := 8;
      --  A tag starts with its type and its size, each four bytes. The
      --  command line's text follows, ending with a zero byte.

      Tag    : Storage_Array (1 .. 512) := (others => 0);
      Params : aliased Rose.Invocation.Invocation_Record;
      Size   : Storage_Count;
      Start  : Storage_Offset;
   begin
      Rose.System_Calls.Initialize_Send (Params, Command_Line);
      Rose.System_Calls.Send_Buffer
        (Params, Tag'Length, Tag'Address, Writable => True);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) <= Header_Size
      then
         return False;
      end if;

      Size :=
        Storage_Count'Min (Tag'Length, Storage_Count (Params.Data (0)));

      Start := Tag'First + Header_Size;
      while Start <= Size loop
         declare
            Finish : Storage_Offset := Start;
         begin
            while Finish <= Size
              and then Tag (Finish) /= 0
              and then Tag (Finish) /= Character'Pos (' ')
            loop
               Finish := Finish + 1;
            end loop;

            if Natural (Finish - Start) = Name'Length then
               declare
                  Same : Boolean := True;
               begin
                  for I in Name'Range loop
                     if Tag (Start + Storage_Offset (I - Name'First))
                       /= Character'Pos (Name (I))
                     then
                        Same := False;
                     end if;
                  end loop;
                  if Same then
                     return True;
                  end if;
               end;
            end if;

            exit when Finish > Size or else Tag (Finish) = 0;
            Start := Finish + 1;
         end;
      end loop;

      return False;
   end Has_Argument;

   ---------------------------
   -- Interrupt_Handler_Cap --
   ---------------------------

   overriding function Interrupt_Handler_Cap
     (Item : in out Interrupt_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Devices.Interrupt_Handler.Server.Cap_Of
         (Context, Item));

   -------------
   -- Link_Up --
   -------------

   overriding function Link_Up
     (Item : in out Card_Object)
      return Boolean
   is
      pragma Unreferenced (Item);
   begin

      --  The card's status, VIRTIO_NET_F_STATUS, isn't negotiated, so
      --  the link is up while the card is.

      return Has_Card;
   end Link_Up;

   -----------------
   -- Mac_Address --
   -----------------

   overriding function Mac_Address
     (Item : in out Card_Object)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Item);
   begin
      return Virtio_Net.Device.MAC_Address;
   end Mac_Address;

   ------------------------
   -- Network_Device_Cap --
   ------------------------

   overriding function Network_Device_Cap
     (Item : in out Card_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Devices.Network_Device.Server.Cap_Of
         (Context, Item));

   ------------
   -- Notify --
   ------------

   overriding procedure Notify
     (Item   : in out Card_Object;
      Signal : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Item);
   begin
      if Signal_Cap /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Delete_Cap (Signal_Cap);
      end if;
      Signal_Cap := Signal;

      if Virtio_Net.Device.Frame_Waiting then
         Raise_Signal;
      end if;
   end Notify;

   ------------------
   -- Open_Console --
   ------------------

   function Open_Console return Boolean is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Console_Writer_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         Rose.Standard_Streams.Set (Standard_Output => Params.Caps (0));
         return True;
      else
         return False;
      end if;
   end Open_Console;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
   begin
      Ada.Text_IO.Put_Line (Text);
      Ada.Text_IO.Flush;
   end Put_Line;

   ------------------
   -- Raise_Signal --
   ------------------

   procedure Raise_Signal is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Signal_Cap /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Initialize_Send (Params, Signal_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end if;
   end Raise_Signal;

   -------------------
   -- Receive_Frame --
   -------------------

   overriding procedure Receive_Frame
     (Item  : in out Card_Object;
      Frame : out Ada.Streams.Stream_Element_Array;
      Last  : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Item);
   begin
      Virtio_Net.Device.Receive (Frame, Last);
      Acknowledge;
   end Receive_Frame;

   ------------------
   -- Register_IRQ --
   ------------------

   --  The kernel sends the interrupt to the Handle_Interrupt endpoint
   --  itself. Identifier 0 is the first object attached to the
   --  Interrupt_Handler server, which is The_Interrupt.

   procedure Register_IRQ is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Interrupt_Cap);
      Rose.System_Calls.Send_Cap
        (Params,
         Rose.Server.Create_Endpoint
           (Rose.Interfaces.Devices.Interrupt_Handler
             .Handle_Interrupt_Endpoint));
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         Ack_Cap := Params.Caps (0);
      else
         Put_Line
           ("virtio-net: no cap to acknowledge the interrupt");
      end if;
   end Register_IRQ;

   --------------------
   -- Restorable_Cap --
   --------------------

   overriding function Restorable_Cap
     (Item : in out Card_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Restorable.Server.Cap_Of (Context, Item));

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Card_Object) is
      pragma Unreferenced (Item);
   begin

      --  The checkpoint has the driver, but not the card, which has been
      --  reset, nor the DMA pages, which are never saved (ADR 0007,
      --  section 6). The kernel registers the interrupt again itself.

      if Has_Card then
         Test := Off;
         Send_Second := False;
         Virtio_Net.Device.Map_Pages;
         Enable_PCI;
         Virtio_Net.Device.Start (Has_Card);
         if not Has_Card then
            Put_Line ("virtio-net: the card did not start");
         end if;
      end if;
   end Restored;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test is

      function Is_ARP_Reply (Frame : Stream_Element_Array) return Boolean
      is (Frame'Length >= 42
          and then Frame (Frame'First + 12 .. Frame'First + 13)
                   = (16#08#, 16#06#)
          and then Frame (Frame'First + 20 .. Frame'First + 21)
                   = (16#00#, 16#02#)
          and then Frame (Frame'First + 28 .. Frame'First + 31)
                   = (10, 0, 2, 2));
      --  An ARP reply from 10.0.2.2

      Frame : Stream_Element_Array (1 .. Virtio_Net.Device.Max_Frame_Size);
      Last  : Stream_Element_Offset;
   begin
      while Virtio_Net.Device.Frame_Waiting loop
         Virtio_Net.Device.Receive (Frame, Last);
         if Is_ARP_Reply (Frame (1 .. Last)) then
            case Test is
               when Waiting_First =>
                  Put_Line
                    ("virtio-net test: ARP reply from 10.0.2.2: ok");
                  Test := Waiting_Second;
                  Send_Second := True;
               when Waiting_Second =>
                  Put_Line
                    ("virtio-net test: "
                     & "another reply after the acknowledge: ok");
                  Test := Done;
               when Off | Done =>
                  null;
            end case;
         end if;
      end loop;
   end Run_Test;

   ----------------------
   -- Send_ARP_Request --
   ----------------------

   procedure Send_ARP_Request is
      use type Rose.Words.Word_64;
      MAC   : constant Rose.Words.Word_64 := Virtio_Net.Device.MAC_Address;
      Frame : Stream_Element_Array (1 .. 60) := (others => 0);
      Sent  : Boolean;

      procedure Put_MAC (First : Stream_Element_Offset);
      --  The card's address, in Frame from First

      -------------
      -- Put_MAC --
      -------------

      procedure Put_MAC (First : Stream_Element_Offset) is
      begin
         for I in Stream_Element_Offset range 0 .. 5 loop
            Frame (First + I) :=
              Stream_Element (MAC / 2 ** Natural (8 * (5 - I)) mod 256);
         end loop;
      end Put_MAC;

   begin
      Frame (1 .. 6) := (others => 16#FF#);
      Put_MAC (7);
      Frame (13 .. 22) :=
        (16#08#, 16#06#,      --  ARP
         16#00#, 16#01#,      --  Ethernet
         16#08#, 16#00#,      --  IPv4
         6, 4,                --  the sizes of their addresses
         16#00#, 16#01#);     --  a request
      Put_MAC (23);
      Frame (29 .. 32) := (10, 0, 2, 15);
      Frame (39 .. 42) := (10, 0, 2, 2);

      Virtio_Net.Device.Send (Frame, Sent);
      if not Sent then
         Put_Line ("virtio-net test: request sent: FAIL");
      end if;
   end Send_ARP_Request;

   ----------------
   -- Send_Frame --
   ----------------

   overriding procedure Send_Frame
     (Item  : in out Card_Object;
      Frame : Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Item);
      Sent : Boolean := False;
   begin
      if Has_Card then
         Virtio_Net.Device.Send (Frame, Sent);
      end if;
      Acknowledge;
      if not Sent then
         raise Frame_Not_Sent;
      end if;
   end Send_Frame;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Virtio_Net.Server;
