with Ada.Streams;
with Ada.Text_IO;

with Interfaces;

with System;

with Rose.Capabilities;
with Rose.Shared_Regions;
with Rose.Surface_Events;
with Rose.System_Calls.Own_Bank;
with Rose.Words;

with Rose.Interfaces.Processes.Launch;
with Rose.Interfaces.Processes.Process_Control;
with Rose.Interfaces.Storage.Region_Access;
with Rose.Interfaces.Streams.Reader;
with Rose.Interfaces.Screens.Surface.Client;
with Rose.Interfaces.Screens.Test_Surface.Client;

with Tests.Registry;

package body Tests.Screen is

   use Rose.Words;
   use type Rose.Capabilities.Capability;

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": " & Message);
      return False;
   end Fail;

   -----------------
   -- Screen_Test --
   -----------------

   function Screen_Test return Boolean is
      use type Interfaces.Integer_32;
   begin
      Ada.Text_IO.Put ("Screen test");

      if Tests.Registry.Screen = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no screen)");
         return True;
      end if;

      declare
         Surface : Rose.Interfaces.Screens.Surface.Client.Surface_Client :=
                     Rose.Interfaces.Screens.Surface.Client.From_Cap
                       (Tests.Registry.Screen);
         Width, Height : Word;
      begin
         Surface.Get_Size (Width, Height);
         if Width = 0 or else Height = 0 then
            return Fail ("the screen has no size");
         end if;

         declare
            Over    : Boolean;
            X, Y    : Interfaces.Integer_32;
            Buttons : Word;
         begin
            --  The surface is the whole screen, so the pointer is over
            --  it, and on it (#588). Its buttons may be down.

            Surface.Get_Pointer (Over, X, Y, Buttons);
            if not Over then
               return Fail ("the pointer isn't over the screen");
            elsif X < 0 or else Y < 0
              or else X >= Interfaces.Integer_32 (Width)
              or else Y >= Interfaces.Integer_32 (Height)
            then
               return Fail ("the pointer is off the screen");
            elsif Buttons >= 32 then
               return Fail ("the pointer has more than five buttons");
            end if;
         end;

         --  The console holds the screen (#653), so the test maps the
         --  buffer, but neither draws on it nor reads the events, which are
         --  the console's.

         declare
            Region  : Rose.Interfaces.Storage.Region_Access
                          .Region_Access_Object'Class :=
                        Surface.Get_Buffer;
            Address : constant System.Address :=
                        Rose.Shared_Regions.Map (Region);
         begin
            Rose.Shared_Regions.Unmap (Address);
         end;
      end;

      return True;
   end Screen_Test;

   -----------------------
   -- Test_Surface_Test --
   -----------------------

   function Test_Surface_Test return Boolean is
      use Ada.Streams;
      use type System.Address;

      Launcher : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
                   Tests.Registry.Launcher ("memory_surface");
      Process  : Rose.Interfaces.Processes.Process_Control
        .Process_Control_Object'Class :=
          Launcher.Launch (Rose.System_Calls.Own_Bank.Cap);
      Surface  : Rose.Interfaces.Screens.Test_Surface.Client
        .Test_Surface_Client;
      Width, Height : Word;

      function Next_Event
        (Kind, First, Second : Word_32)
         return Boolean;
      --  True if the next event that the surface's events give is Kind,
      --  with First and Second

      ----------------
      -- Next_Event --
      ----------------

      function Next_Event
        (Kind, First, Second : Word_32)
         return Boolean
      is
         Events : Rose.Interfaces.Streams.Reader.Reader_Object'Class :=
                    Surface.Events;
         Buffer : Stream_Element_Array
           (1 .. Rose.Surface_Events.Event_Bytes);
         Last   : Stream_Element_Offset;
      begin
         Events.Read (Buffer, Last);
         return Last = Buffer'Last
           and then Rose.Surface_Events.Word_At (Buffer, 1) = Kind
           and then Rose.Surface_Events.Word_At (Buffer, 5) = First
           and then Rose.Surface_Events.Word_At (Buffer, 9) = Second;
      end Next_Event;

      Passed : Boolean := True;

   begin
      Ada.Text_IO.Put ("Test surface test");

      Rose.Interfaces.Screens.Test_Surface.Client.Open
        (Surface, Process.Published_Interface);

      Surface.Get_Size (Width, Height);
      if Width /= 320 or else Height /= 200 then
         Passed := Fail ("it isn't 320 by 200");
      end if;

      if Passed then
         declare
            Region  : Rose.Interfaces.Storage.Region_Access
                          .Region_Access_Object'Class :=
                        Surface.Get_Buffer;
            Address : constant System.Address :=
                        Rose.Shared_Regions.Map (Region);
            Pixels  : array (0 .. 320 * 200 - 1) of Word_32
              with Import, Address => Address;
         begin
            if Address = System.Null_Address then
               Passed := Fail ("its buffer doesn't map");
            else
               Pixels (Pixels'Last) := 16#00FF_8040#;
               Passed := Pixels (Pixels'Last) = 16#00FF_8040#
                 or else Fail ("its buffer can't be written");
               Rose.Shared_Regions.Unmap (Address);
            end if;
         end;
      end if;

      if Passed then
         Surface.Inject (Rose.Surface_Events.Key_Event, 16#61#, 0, 0);
         if not Next_Event (Rose.Surface_Events.Key_Event, 16#61#, 0) then
            Passed := Fail ("an injected key wasn't read back");
         end if;
      end if;

      if Passed then
         Surface.Set_Size (160, 100);
         Surface.Get_Size (Width, Height);
         if Width /= 160 or else Height /= 100 then
            Passed := Fail ("Set_Size didn't change its size");
         elsif not Next_Event (Rose.Surface_Events.Size_Event, 160, 100) then
            Passed := Fail ("Set_Size didn't queue a Size event");
         end if;
      end if;

      Process.Destroy;
      return Passed;
   end Test_Surface_Test;

end Tests.Screen;
