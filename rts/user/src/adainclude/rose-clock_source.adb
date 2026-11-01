package body Rose.Clock_Source is

   The_Source : Reader := null;

   --------------
   -- Realtime --
   --------------

   function Realtime return Rose.Words.Word_64 is
   begin
      if The_Source = null then
         raise Program_Error with "the clock has not been opened";
      end if;
      return The_Source.all;
   end Realtime;

   ---------
   -- Set --
   ---------

   procedure Set (Source : Reader) is
   begin
      The_Source := Source;
   end Set;

end Rose.Clock_Source;
