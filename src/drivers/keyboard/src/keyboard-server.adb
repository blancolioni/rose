with Rose.Invocation;
with Rose.Keys;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Event_Listener.Client;
with Rose.Interfaces.Event_Source.Server;
with Rose.Interfaces.Interrupt_Handler.Server;

with Rose.Devices.Port_IO;

with Keyboard.Codes;

package body Keyboard.Server is

   use type Rose.Words.Word_8;

   --  One object serves Event_Source, which the console adds itself to as
   --  a listener, and another takes the keyboard's interrupt.

   type Source_Object is
     new Rose.Interfaces.Event_Source.Server.Event_Source_Interface
     with null record;

   overriding function Event_Source_Cap
     (Item : in out Source_Object)
      return Rose.Capabilities.Capability;

   overriding procedure Add_Listener
     (Item     : in out Source_Object;
      Listener : in out
        Rose.Interfaces.Event_Listener.Event_Listener_Object'Class);

   type Interrupt_Object is
     new Rose.Interfaces.Interrupt_Handler.Server.Interrupt_Handler_Interface
     with null record;

   overriding function Interrupt_Handler_Cap
     (Item : in out Interrupt_Object)
      return Rose.Capabilities.Capability;

   overriding procedure Handle_Interrupt
     (Item : in out Interrupt_Object;
      Code : Rose.Words.Word);

   Context       : Rose.Server.Server_Context;
   The_Source    : aliased Source_Object;
   The_Interrupt : aliased Interrupt_Object;

   Have_Listener : Boolean := False;
   Listener      : Rose.Interfaces.Event_Listener.Client
     .Event_Listener_Client;
   --  The first listener, which gets every key. A user-target client
   --  first invokes its cap on its first call, at the first key, so
   --  opening it in Add_Listener does not call the listener, which is
   --  waiting for the reply to Add_Listener.

   Keyboard_Pending : constant := 16#01#;

   procedure Register_IRQ;

   ------------------
   -- Add_Listener --
   ------------------

   overriding procedure Add_Listener
     (Item     : in out Source_Object;
      Listener : in out
        Rose.Interfaces.Event_Listener.Event_Listener_Object'Class)
   is
      pragma Unreferenced (Item);
   begin
      if not Have_Listener then
         Rose.Interfaces.Event_Listener.Client.Open
           (Server.Listener, Listener.Event_Listener_Cap);
         Have_Listener := True;
      end if;
   end Add_Listener;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Interrupt_Handler.Server.Attach
        (Context, The_Interrupt'Access);
      Register_IRQ;
      Rose.Interfaces.Event_Source.Server.Publish
        (Context, The_Source'Access);
   end Create_Server;

   ----------------------
   -- Event_Source_Cap --
   ----------------------

   overriding function Event_Source_Cap
     (Item : in out Source_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Event_Source.Server.Cap_Of (Item));

   ----------------------
   -- Handle_Interrupt --
   ----------------------

   overriding procedure Handle_Interrupt
     (Item : in out Interrupt_Object;
      Code : Rose.Words.Word)
   is
      pragma Unreferenced (Item, Code);

      function Key_Code_Available return Boolean
      is ((Rose.Devices.Port_IO.Port_In_8 (Read_Status_Cap)
           and Keyboard_Pending) /= 0);

   begin

      --  The interrupt has no reply: the kernel sends it, and doesn't
      --  wait.

      while Key_Code_Available loop
         declare
            Result : Keyboard.Codes.Key_Press_Array (1 .. 4);
            Last   : Natural;
         begin

            --  The keyboard gives keys, each with its modifiers in one
            --  word (Rose.Keys.Pack). The console's line discipline echoes
            --  them (#397, #454, #734).

            Keyboard.Codes.Handle_Key
              (Scan_Code => Rose.Devices.Port_IO.Port_In_8 (Read_Key_Cap),
               Result    => Result,
               Last      => Last);

            if Have_Listener then
               for Press of Result (1 .. Last) loop
                  Listener.On_Event
                    (Rose.Keys.Pack (Press.Key, Press.Modifiers));
               end loop;
            end if;
         end;
      end loop;
   end Handle_Interrupt;

   ---------------------------
   -- Interrupt_Handler_Cap --
   ---------------------------

   overriding function Interrupt_Handler_Cap
     (Item : in out Interrupt_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Interrupt_Handler.Server.Cap_Of (Item));

   ------------------
   -- Register_IRQ --
   ------------------

   --  The kernel sends the interrupt to the Handle_Interrupt endpoint
   --  itself. Identifier 0 is the first object attached to the
   --  Interrupt_Handler server, which is The_Interrupt.

   procedure Register_IRQ is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Register_IRQ_Cap);
      Rose.System_Calls.Send_Cap
        (Params,
         Rose.Server.Create_Endpoint
           (Rose.Interfaces.Interrupt_Handler.Handle_Interrupt_Endpoint));
      Rose.System_Calls.Invoke_Capability (Params);
   end Register_IRQ;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Keyboard.Server;
