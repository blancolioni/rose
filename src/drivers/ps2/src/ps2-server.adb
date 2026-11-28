with Rose.Invocation;
with Rose.Keys;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Devices.Event_Listener.Client;
with Rose.Interfaces.Devices.Input_Controller.Server;
with Rose.Interfaces.Devices.Interrupt_Handler.Server;
with Rose.Interfaces.Devices.Pointer_Listener.Client;
with Rose.Interfaces.Processes.Restorable.Server;

with Rose.Devices.Port_IO;

with Mouse_Packets;
with PS2.Key_Codes;

package body PS2.Server is

   use type Rose.Words.Word_8;

   --  One object is the Input_Controller, whose ancestors are the
   --  keyboard's Event_Source and the mouse's Pointer_Source, and which is
   --  Restorable, so that the mouse is set up again after a restore.
   --  Another takes both interrupts.

   type Controller_Object is limited new
     Rose.Interfaces.Devices.Input_Controller.Server
       .Input_Controller_Interface
     and Rose.Interfaces.Processes.Restorable.Server.Restorable_Interface
     with null record;

   overriding function Input_Controller_Cap
     (Item : in out Controller_Object)
      return Rose.Capabilities.Capability;

   overriding function Event_Source_Cap
     (Item : in out Controller_Object)
      return Rose.Capabilities.Capability;

   overriding function Pointer_Source_Cap
     (Item : in out Controller_Object)
      return Rose.Capabilities.Capability;

   overriding function Restorable_Cap
     (Item : in out Controller_Object)
      return Rose.Capabilities.Capability;

   overriding procedure Add_Listener
     (Item     : in out Controller_Object;
      Listener : in out
        Rose.Interfaces.Devices.Event_Listener.Event_Listener_Object'Class);

   overriding procedure Add_Pointer_Listener
     (Item     : in out Controller_Object;
      Listener : in out
        Rose.Interfaces.Devices.Pointer_Listener
          .Pointer_Listener_Object'Class);

   overriding procedure Restored (Item : in out Controller_Object);

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

   Context        : Rose.Server.Server_Context;
   The_Controller : aliased Controller_Object;
   The_Interrupt  : aliased Interrupt_Object;

   Have_Listener : Boolean := False;
   Listener      : Rose.Interfaces.Devices.Event_Listener.Client
     .Event_Listener_Client;
   --  The first listener, which gets every key. A user-target client
   --  first invokes its cap on its first call, at the first key, so
   --  opening it in Add_Listener does not call the listener, which is
   --  waiting for the reply to Add_Listener.

   Have_Pointer_Listener : Boolean := False;
   Pointer_Listener      : Rose.Interfaces.Devices.Pointer_Listener.Client
     .Pointer_Listener_Client;
   --  The same for the mouse.

   Mouse_Present : Boolean := False;
   Decoder       : Mouse_Packets.Decoder;
   Combiner      : Mouse_Packets.Combiner;

   --  The controller's status port.

   Output_Full : constant := 16#01#;
   --  A byte is waiting in the data port.
   Input_Full  : constant := 16#02#;
   --  The controller hasn't yet taken the last byte written to it.
   Aux_Data    : constant := 16#20#;
   --  The waiting byte is from the mouse.

   --  Commands to the controller, on the command port.

   Read_Configuration  : constant := 16#20#;
   Write_Configuration : constant := 16#60#;
   Enable_Aux_Port     : constant := 16#A8#;
   Write_To_Aux        : constant := 16#D4#;

   --  The configuration byte.

   Aux_Interrupt     : constant := 16#02#;
   Aux_Clock_Disable : constant := 16#20#;

   --  Commands to the mouse, and its reply.

   Set_Sample_Rate  : constant := 16#F3#;
   Get_Device_Id    : constant := 16#F2#;
   Enable_Reporting : constant := 16#F4#;
   Set_Defaults     : constant := 16#F6#;
   Acknowledge      : constant := 16#FA#;

   Wheel_Mouse_Id   : constant := 3;

   Attempts : constant := 20_000;
   --  How many times to read the status port before giving up on a
   --  byte, so that a controller with no mouse can't stop the boot.

   procedure Register_IRQ (Cap : Rose.Capabilities.Capability);

   procedure Set_Up_Mouse;
   --  Enable the controller's second port and its interrupt, and the
   --  mouse's reporting, by polling, with the interrupts not yet taken
   --  (or, after a restore, not yet handled). Mouse_Present is True if the
   --  mouse answered; the decoder expects four-byte packets if it has a
   --  wheel.

   function Status return Rose.Words.Word_8
   is (Rose.Devices.Port_IO.Port_In_8 (Read_Status_Cap));

   procedure Send_Pointer (Event : Mouse_Packets.Pointer_Event);

   ------------------
   -- Add_Listener --
   ------------------

   overriding procedure Add_Listener
     (Item     : in out Controller_Object;
      Listener : in out
        Rose.Interfaces.Devices.Event_Listener.Event_Listener_Object'Class)
   is
      pragma Unreferenced (Item);
   begin
      if not Have_Listener then
         Rose.Interfaces.Devices.Event_Listener.Client.Open
           (Server.Listener, Listener.Event_Listener_Cap);
         Have_Listener := True;
      end if;
   end Add_Listener;

   --------------------------
   -- Add_Pointer_Listener --
   --------------------------

   overriding procedure Add_Pointer_Listener
     (Item     : in out Controller_Object;
      Listener : in out
        Rose.Interfaces.Devices.Pointer_Listener
          .Pointer_Listener_Object'Class)
   is
      pragma Unreferenced (Item);
   begin
      if not Have_Pointer_Listener then
         Rose.Interfaces.Devices.Pointer_Listener.Client.Open
           (Server.Pointer_Listener, Listener.Pointer_Listener_Cap);
         Have_Pointer_Listener := True;
      end if;
   end Add_Pointer_Listener;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Set_Up_Mouse;
      Rose.Interfaces.Devices.Interrupt_Handler.Server.Attach
        (Context, The_Interrupt'Access);
      Register_IRQ (Register_IRQ_Cap);
      Register_IRQ (Register_Mouse_Cap);
      Rose.Interfaces.Devices.Input_Controller.Server.Publish
        (Context, The_Controller'Access);
      Rose.Interfaces.Processes.Restorable.Server.Attach
        (Context, The_Controller'Access);
   end Create_Server;

   ----------------------
   -- Event_Source_Cap --
   ----------------------

   overriding function Event_Source_Cap
     (Item : in out Controller_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Devices.Input_Controller.Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Devices.Input_Controller.Event_Source_Interface));

   ----------------------
   -- Handle_Interrupt --
   ----------------------

   overriding procedure Handle_Interrupt
     (Item : in out Interrupt_Object;
      Code : Rose.Words.Word)
   is
      pragma Unreferenced (Item, Code);
   begin

      --  The interrupt has no reply: the kernel sends it, and doesn't
      --  wait. Both interrupts come here, and every byte waiting is
      --  read, whichever interrupt it was: the status says whose it is.

      loop
         declare
            State : constant Rose.Words.Word_8 := Status;
            Value : Rose.Words.Word_8;
         begin
            exit when (State and Output_Full) = 0;
            Value := Rose.Devices.Port_IO.Port_In_8 (Read_Key_Cap);

            if (State and Aux_Data) /= 0 then
               if Mouse_Present then
                  declare
                     Complete : Boolean;
                     Event    : Mouse_Packets.Pointer_Event;
                     Ready    : Mouse_Packets.Event_Array (1 .. 2);
                     Last     : Natural;
                  begin
                     Mouse_Packets.Put
                       (Decoder, Mouse_Packets.Byte (Value),
                        Complete, Event);
                     if Complete then
                        Mouse_Packets.Add (Combiner, Event, Ready, Last);
                        for Item of Ready (1 .. Last) loop
                           Send_Pointer (Item);
                        end loop;
                     end if;
                  end;
               end if;
            else
               declare
                  Result : PS2.Key_Codes.Key_Press_Array (1 .. 4);
                  Last   : Natural;
               begin

                  --  The keyboard gives keys, each with its modifiers in
                  --  one word (Rose.Keys.Pack). The console's line
                  --  discipline echoes them (#397, #454, #734).

                  PS2.Key_Codes.Handle_Key
                    (Scan_Code => Value,
                     Result    => Result,
                     Last      => Last);

                  if Have_Listener then
                     for Press of Result (1 .. Last) loop
                        Listener.On_Event
                          (Rose.Keys.Pack (Press.Key, Press.Modifiers));
                     end loop;
                  end if;
               end;
            end if;
         end;
      end loop;

      --  The movement of this pass, in one call (#588).

      declare
         Event : Mouse_Packets.Pointer_Event;
         Ready : Boolean;
      begin
         Mouse_Packets.Flush (Combiner, Event, Ready);
         if Ready then
            Send_Pointer (Event);
         end if;
      end;
   end Handle_Interrupt;

   --------------------------
   -- Input_Controller_Cap --
   --------------------------

   overriding function Input_Controller_Cap
     (Item : in out Controller_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Devices.Input_Controller.Server.Cap_Of
         (Context, Item));

   ---------------------------
   -- Interrupt_Handler_Cap --
   ---------------------------

   overriding function Interrupt_Handler_Cap
     (Item : in out Interrupt_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Devices.Interrupt_Handler.Server.Cap_Of
         (Context, Item));

   ------------------------
   -- Pointer_Source_Cap --
   ------------------------

   overriding function Pointer_Source_Cap
     (Item : in out Controller_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Devices.Input_Controller.Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Devices.Input_Controller.Pointer_Source_Interface));

   ------------------
   -- Register_IRQ --
   ------------------

   --  The kernel sends the interrupt to the Handle_Interrupt endpoint
   --  itself. Identifier 0 is the first object attached to the
   --  Interrupt_Handler server, which is The_Interrupt. Both interrupts
   --  go to it.

   procedure Register_IRQ (Cap : Rose.Capabilities.Capability) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Cap);
      Rose.System_Calls.Send_Cap
        (Params,
         Rose.Server.Create_Endpoint
           (Rose.Interfaces.Devices.Interrupt_Handler
             .Handle_Interrupt_Endpoint));
      Rose.System_Calls.Invoke_Capability (Params);
   end Register_IRQ;

   --------------------
   -- Restorable_Cap --
   --------------------

   overriding function Restorable_Cap
     (Item : in out Controller_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Restorable.Server.Cap_Of (Context, Item));

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Controller_Object) is
      pragma Unreferenced (Item);
   begin

      --  The checkpoint has the driver, but not the hardware: on a new
      --  boot, the controller's second port is off again. The keyboard
      --  needs nothing.

      Set_Up_Mouse;
   end Restored;

   ------------------
   -- Send_Pointer --
   ------------------

   procedure Send_Pointer (Event : Mouse_Packets.Pointer_Event) is
   begin
      if Have_Pointer_Listener then
         Pointer_Listener.On_Pointer
           (Event.DX, Event.DY, Event.DZ, Rose.Words.Word (Event.Buttons));
      end if;
   end Send_Pointer;

   ------------------
   -- Set_Up_Mouse --
   ------------------

   procedure Set_Up_Mouse is

      function Input_Empty return Boolean;
      --  Wait until the controller has taken the last byte written to it,
      --  or give up.

      procedure Command (Value : Rose.Words.Word_8);
      --  Write Value to the command port.

      procedure Data (Value : Rose.Words.Word_8);
      --  Write Value to the data port.

      procedure Read
        (From_Mouse : Boolean;
         Value      : out Rose.Words.Word_8;
         Ok         : out Boolean);
      --  Wait for a byte from the mouse, or from the controller itself.
      --  A byte from the other is dropped: a key typed now is lost.

      function To_Mouse (Value : Rose.Words.Word_8) return Boolean;
      --  Send Value to the mouse; True if it acknowledged it.

      -------------
      -- Command --
      -------------

      procedure Command (Value : Rose.Words.Word_8) is
      begin
         if Input_Empty then
            Rose.Devices.Port_IO.Port_Out_8 (Command_Cap, Value);
         end if;
      end Command;

      ----------
      -- Data --
      ----------

      procedure Data (Value : Rose.Words.Word_8) is
      begin
         if Input_Empty then
            Rose.Devices.Port_IO.Port_Out_8 (Data_Cap, Value);
         end if;
      end Data;

      -----------------
      -- Input_Empty --
      -----------------

      function Input_Empty return Boolean is
      begin
         for Attempt in 1 .. Attempts loop
            if (Status and Input_Full) = 0 then
               return True;
            end if;
         end loop;
         return False;
      end Input_Empty;

      ----------
      -- Read --
      ----------

      procedure Read
        (From_Mouse : Boolean;
         Value      : out Rose.Words.Word_8;
         Ok         : out Boolean)
      is
      begin
         Value := 0;
         Ok := False;
         for Attempt in 1 .. Attempts loop
            declare
               State : constant Rose.Words.Word_8 := Status;
            begin
               if (State and Output_Full) /= 0 then
                  Value := Rose.Devices.Port_IO.Port_In_8 (Read_Key_Cap);
                  if ((State and Aux_Data) /= 0) = From_Mouse then
                     Ok := True;
                     return;
                  end if;
               end if;
            end;
         end loop;
      end Read;

      --------------
      -- To_Mouse --
      --------------

      function To_Mouse (Value : Rose.Words.Word_8) return Boolean is
         Reply : Rose.Words.Word_8;
         Ok    : Boolean;
      begin
         Command (Write_To_Aux);
         Data (Value);
         Read (True, Reply, Ok);
         return Ok and then Reply = Acknowledge;
      end To_Mouse;

      type Rate_Array is array (1 .. 3) of Rose.Words.Word_8;
      Knock : constant Rate_Array := (200, 100, 80);

      Configuration : Rose.Words.Word_8;
      Id            : Rose.Words.Word_8;
      Ok            : Boolean;

   begin
      Mouse_Present := False;
      Mouse_Packets.Reset (Decoder, Wheel => False);
      Mouse_Packets.Reset (Combiner);

      --  Drop whatever is waiting, so that the replies read below are
      --  the replies.

      for I in 1 .. 32 loop
         exit when (Status and Output_Full) = 0;
         declare
            Value : constant Rose.Words.Word_8 :=
              Rose.Devices.Port_IO.Port_In_8 (Read_Key_Cap);
            pragma Unreferenced (Value);
         begin
            null;
         end;
      end loop;

      Command (Enable_Aux_Port);

      Command (Read_Configuration);
      Read (False, Configuration, Ok);
      if not Ok then
         return;
      end if;

      Configuration :=
        (Configuration or Aux_Interrupt) and not Aux_Clock_Disable;
      Command (Write_Configuration);
      Data (Configuration);

      if not To_Mouse (Set_Defaults) then
         return;
      end if;

      --  The IntelliMouse's knock: sample rates 200, 100 and 80, and then
      --  a mouse with a wheel says that its id is 3, and sends four-byte
      --  packets.

      for Rate of Knock loop
         exit when not To_Mouse (Set_Sample_Rate)
           or else not To_Mouse (Rate);
      end loop;

      if To_Mouse (Get_Device_Id) then
         Read (True, Id, Ok);
         Mouse_Packets.Reset
           (Decoder, Wheel => Ok and then Id = Wheel_Mouse_Id);
      end if;

      Mouse_Present := To_Mouse (Enable_Reporting);
   end Set_Up_Mouse;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end PS2.Server;
