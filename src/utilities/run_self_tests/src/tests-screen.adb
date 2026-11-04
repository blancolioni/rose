with Ada.Text_IO;

with Interfaces;

with System;

with Rose.Capabilities;
with Rose.Shared_Regions;
with Rose.Words;

with Rose.Interfaces.Region;
with Rose.Interfaces.Stream_Reader;
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

         declare
            Events : constant Rose.Interfaces.Stream_Reader
              .Stream_Reader_Object'Class := Surface.Events;
            pragma Unreferenced (Events);
            --  Not read: there are no events yet, and a Read would wait.

            Region  : Rose.Interfaces.Region.Region_Object'Class :=
                        Surface.Get_Buffer;
            Address : constant System.Address :=
                        Rose.Shared_Regions.Map (Region);

            type Pixel_Array is array (Word range <>) of Word_32;
            Pixels : Pixel_Array (0 .. Width * Height - 1)
              with Import, Address => Address;
         begin
            for Row in 0 .. Height - 1 loop
               for Column in 0 .. Width - 1 loop
                  Pixels (Row * Width + Column) :=
                    (Column mod 256) * 2 ** 16
                    + (Row mod 256) * 2 ** 8 + 16#80#;
               end loop;
            end loop;

            Surface.Damage (0, 0, Width, Height);
            Rose.Shared_Regions.Unmap (Address);
         end;
      end;

      return True;
   end Screen_Test;

end Tests.Screen;
