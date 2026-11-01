------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . E X C E P T I O N S . L A S T _ C H A N C E _ H A N D L E R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  The Rose version (#170): report an exception that nothing handles
--  through Rose.Last_Chance, as Last_Chance_Handler does for a raise in a
--  program compiled with No_Exception_Propagation, and exit with status 1.

with System.Standard_Library;

with Rose.Last_Chance;

procedure Ada.Exceptions.Last_Chance_Handler
  (Except : Exception_Occurrence)
is
   use type Rose.Last_Chance.Failure_Handler;

   procedure Exit_Process (Status : Integer);
   pragma Import (C, Exit_Process, "exit");
   pragma No_Return (Exit_Process);

   Handler : constant Rose.Last_Chance.Failure_Handler :=
               Rose.Last_Chance.Handler;

begin
   if Handler /= null then
      declare
         Message : constant String := Exception_Message (Except);
      begin
         if Message = "" then
            Handler ("unhandled exception " & Exception_Name (Except));
         else
            Handler ("unhandled exception " & Exception_Name (Except)
                     & ": " & Message);
         end if;
      end;
   end if;

   System.Standard_Library.Adafinal;
   Exit_Process (1);
end Ada.Exceptions.Last_Chance_Handler;
