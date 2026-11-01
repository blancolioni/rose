--                              -*- Mode: Ada -*-
--  Filename        : last_chance_handler.adb
--  Description     : Implementation of the exception handler for the kernel.
--  Author          : Luke A. Guest
--  Created On      : Thu Jun 14 12:06:48 2012
--  Licence         : See LICENCE in the root directory.

with Rose.Last_Chance;

procedure Last_Chance_Handler
  (Source_Location : System.Address; Line : Integer)
is
   use type Rose.Last_Chance.Failure_Handler;

   procedure Exit_Process (Status : Integer);
   pragma Import (C, Exit_Process, "exit");
   pragma No_Return (Exit_Process);

   Handler : constant Rose.Last_Chance.Failure_Handler :=
     Rose.Last_Chance.Handler;

   Location : String (1 .. 200);
   for Location'Address use Source_Location;
   pragma Import (Ada, Location);
   --  The file name, ending with a NUL.

   Message : String (1 .. 240);
   Last    : Natural := 0;

   procedure Add (Text : String);

   ---------
   -- Add --
   ---------

   procedure Add (Text : String) is
   begin
      for Ch of Text loop
         if Last < Message'Last then
            Last := Last + 1;
            Message (Last) := Ch;
         end if;
      end loop;
   end Add;

begin
   if Handler /= null then
      Add ("unhandled exception at ");
      for Ch of Location loop
         exit when Ch = Character'Val (0);
         Add ((1 => Ch));
      end loop;
      Add (":");

      declare
         Digits_Text : String (1 .. 12);
         First       : Positive := Digits_Text'Last + 1;
         Value       : Natural := (if Line < 0 then 0 else Line);
      begin
         loop
            First := First - 1;
            Digits_Text (First) := Character'Val (48 + Value mod 10);
            Value := Value / 10;
            exit when Value = 0;
         end loop;
         Add (Digits_Text (First .. Digits_Text'Last));
      end;

      Handler (Message (1 .. Last));
   end if;

   Exit_Process (1);
end Last_Chance_Handler;
