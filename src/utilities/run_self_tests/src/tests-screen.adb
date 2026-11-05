with Ada.Text_IO;

with Interfaces;

with System;

with Rose.Capabilities;
with Rose.Shared_Regions;
with Rose.Words;

with Rose.Interfaces.Region;
with Rose.Interfaces.Surface.Client;

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
         Surface : Rose.Interfaces.Surface.Client.Surface_Client :=
                     Rose.Interfaces.Surface.Client.From_Cap
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
            Surface.Get_Pointer (Over, X, Y, Buttons);
            if Over or else X /= 0 or else Y /= 0 or else Buttons /= 0 then
               return Fail ("there is a pointer");
            end if;
         end;

         --  The console holds the screen (#653), so the test maps the
         --  buffer, but neither draws on it nor reads the events, which are
         --  the console's.

         declare
            Region  : Rose.Interfaces.Region.Region_Object'Class :=
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
