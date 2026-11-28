with Ada.Text_IO;

with Interfaces;

with System;

with Rose.Capabilities;
with Rose.Shared_Regions;
with Rose.Words;

with Rose.Interfaces.Storage.Region_Access;
with Rose.Interfaces.Screens.Surface.Client;

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

end Tests.Screen;
