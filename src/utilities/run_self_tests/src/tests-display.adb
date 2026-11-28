with Ada.Streams;
with Ada.Text_IO;

with System;

with Rose.Capabilities;
with Rose.Shared_Regions;
with Rose.Surface_Events;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;
with Rose.Words;

with Rose.Core.Trellis_Client;

with Rose.Interfaces.Objects;
with Rose.Interfaces.Processes.Launch;
with Rose.Interfaces.Processes.Process_Control;
with Rose.Interfaces.Screens.Display.Client;
with Rose.Interfaces.Screens.Surface;
with Rose.Interfaces.Screens.Test_Surface.Client;
with Rose.Interfaces.Storage.Bank;
with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Storage.Region_Access;
with Rose.Interfaces.Streams.Reader;

with Tests.Registry;

package body Tests.Display is

   use Rose.Words;
   use type Rose.Capabilities.Capability;

   Width  : constant := 320;
   Height : constant := 200;

   Black : constant Word_32 := 0;
   White : constant Word_32 := 16#00FF_FFFF#;
   Drawn : constant Word_32 := 16#00AB_CDEF#;

   type Pixel_Array is array (0 .. Width * Height - 1) of Word_32;

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   function Weave (X, Y : Natural) return Word_32;
   --  The background's pixel at X, Y: X's root weave.

   ------------------
   -- Display_Test --
   ------------------

   function Display_Test return Boolean is
      Own       : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                    Rose.Interfaces.Storage.Bank.Client.From_Cap
                      (Rose.System_Calls.Own_Bank.Cap);
      Test_Bank : Rose.Interfaces.Storage.Bank.Bank_Object'Class :=
                    Own.Create_Sub_Bank (0, 0);
      In_Bank   : constant Rose.Capabilities.Capability :=
                    Test_Bank.Bank_Cap;
      Passed    : Boolean := True;

   begin
      Ada.Text_IO.Put ("Display test");

      --  Trellis is signalled by its root when the root has events, so it
      --  needs a cap to make signal caps, which it is given as the test's
      --  own. Run from petal, which has none to give, the test has none
      --  either.

      if Rose.System_Calls.Create_Signal_Capability
        = Rose.Capabilities.Null_Capability
      then
         Ada.Text_IO.Put (" (skipped: no Create_Signal cap)");
         Test_Bank.Destroy;
         return True;
      end if;

      declare
         Surfaces : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
                      Tests.Registry.Launcher ("memory_surface");
         Process  : Rose.Interfaces.Processes.Process_Control
           .Process_Control_Object'Class := Surfaces.Launch (In_Bank);
         Root     : Rose.Interfaces.Screens.Test_Surface.Client
           .Test_Surface_Client;
         Trellis  : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
                      Tests.Registry.Launcher ("trellis");
         Served   : Rose.Capabilities.Capability;
         Display  : Rose.Interfaces.Screens.Display.Client.Display_Client;
      begin
         Rose.Interfaces.Screens.Test_Surface.Client.Open
           (Root, Process.Published_Interface);

         Rose.Core.Trellis_Client.Call
           (Endpoint        => Trellis.Launch_Command (In_Bank),
            Create_Endpoint => Rose.System_Calls.Create_Endpoint_Capability,
            Create_Signal   => Rose.System_Calls.Create_Signal_Capability,
            Root            => Root.Surface_Cap,
            Served          => Served);

         if Served = Rose.Capabilities.Null_Capability then
            Passed := Fail ("trellis didn't start");
         else
            Rose.Interfaces.Screens.Display.Client.Open (Display, Served);
         end if;

         if Passed then
            declare
               Root_Buffer : Rose.Interfaces.Storage.Region_Access
                 .Region_Access_Object'Class := Root.Get_Buffer;
               Root_Address : constant System.Address :=
                                Rose.Shared_Regions.Map (Root_Buffer);
               Screen : Pixel_Array with Import, Address => Root_Address;

               function At_Root (X, Y : Natural) return Word_32
               is (Screen (Y * Width + X));

               function Is_Weave (X, Y, W, H : Natural) return Boolean
               is (for all Row in Y .. Y + H - 1 =>
                     (for all Col in X .. X + W - 1 =>
                        At_Root (Col, Row) = Weave (Col, Row)));

            begin
               --  Step 4: the weave, where no window is.

               if not Is_Weave (0, 0, Width, Height) then
                  Passed := Fail ("the root isn't the weave");
               end if;

               declare
                  Surface : Rose.Interfaces.Screens.Surface
                    .Surface_Object'Class := Display.Create_Surface (100, 100);
                  Got_Width, Got_Height : Word;
               begin

                  --  Step 2: one surface has the whole root.

                  Surface.Get_Size (Got_Width, Got_Height);
                  if Passed
                    and then (Got_Width /= Width or else Got_Height /= Height)
                  then
                     Passed := Fail ("the surface isn't the root's size");
                  end if;

                  --  Step 3: damage reaches the root, and nothing else
                  --  does.

                  if Passed then
                     declare
                        Buffer  : Rose.Interfaces.Storage.Region_Access
                          .Region_Access_Object'Class := Surface.Get_Buffer;
                        Address : constant System.Address :=
                                    Rose.Shared_Regions.Map (Buffer);
                        Pixels  : Pixel_Array
                          with Import, Address => Address;
                     begin
                        for Y in 10 .. 29 loop
                           for X in 10 .. 29 loop
                              Pixels (Y * Width + X) := Drawn;
                           end loop;
                        end loop;
                        Surface.Damage (10, 10, 20, 20);

                        if At_Root (10, 10) /= Drawn
                          or else At_Root (29, 29) /= Drawn
                        then
                           Passed := Fail ("damage didn't reach the root");
                        elsif not Is_Weave (0, 0, Width, 10)
                          or else At_Root (30, 15) /= Weave (30, 15)
                        then
                           Passed := Fail ("more than the damage changed");
                        end if;

                        --  Damage past the surface is cut to it.

                        Surface.Damage (300, 190, 16#FFFF_FFFF#, 100);
                        Rose.Shared_Regions.Unmap (Address);
                     end;
                  end if;

                  --  A second surface is refused, until the split.

                  if Passed then
                     begin
                        declare
                           Second : Rose.Interfaces.Screens.Surface
                             .Surface_Object'Class :=
                               Display.Create_Surface (10, 10);
                        begin
                           Second.Release;
                           Passed := Fail ("a second surface was given");
                        end;
                     exception
                        when Rose.Interfaces.Objects.Invocation_Error =>
                           null;
                     end;
                  end if;

                  --  Step 8: keys reach the surface; the pointer and the
                  --  wheel don't, until #589.

                  if Passed then
                     Root.Inject (Rose.Surface_Events.Pointer_Event, 5, 5, 1);
                     Root.Inject (Rose.Surface_Events.Wheel_Event, 1, 0, 0);
                     Root.Inject (Rose.Surface_Events.Key_Event, 16#62#, 0, 0);

                     declare
                        use Ada.Streams;
                        Events : Rose.Interfaces.Streams.Reader
                          .Reader_Object'Class := Surface.Events;
                        Buffer : Stream_Element_Array
                          (1 .. Rose.Surface_Events.Event_Bytes);
                        Last   : Stream_Element_Offset;
                     begin
                        Events.Read (Buffer, Last);
                        if Last /= Buffer'Last
                          or else Rose.Surface_Events.Word_At (Buffer, 1)
                            /= Rose.Surface_Events.Key_Event
                          or else Rose.Surface_Events.Word_At (Buffer, 5)
                            /= 16#62#
                        then
                           Passed := Fail ("the key didn't come first");
                        end if;
                     end;
                  end if;

                  --  Releasing the surface brings the weave back.

                  Surface.Release;
                  if Passed and then not Is_Weave (0, 0, Width, Height) then
                     Passed := Fail ("the weave didn't come back");
                  end if;
               end;

               Rose.Shared_Regions.Unmap (Root_Address);
            end;
         end if;
      end;

      Test_Bank.Destroy;
      return Passed;
   end Display_Test;

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": " & Message);
      return False;
   end Fail;

   -----------
   -- Weave --
   -----------

   function Weave (X, Y : Natural) return Word_32 is
      Rows : constant array (0 .. 3) of Word_32 :=
               (16#07#, 16#0D#, 16#0B#, 16#0E#);
   begin
      return (if (Rows (Y mod 4) / 2 ** (X mod 4)) mod 2 = 1
              then Black else White);
   end Weave;

end Tests.Display;
