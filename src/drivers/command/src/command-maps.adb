with Ada.Containers.Indefinite_Ordered_Maps;

package body Command.Maps is

   --  The registry: each command's name, and the cap of its Launch
   --  interface (#490). A name is in it at most once: adding a name that
   --  is already there replaces its cap.

   package Command_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Rose.Capabilities.Capability,
        "="          => Rose.Capabilities."=");

   Commands : Command_Maps.Map;

   ------------
   -- Delete --
   ------------

   procedure Delete (Command : String) is
   begin
      Commands.Exclude (Command);
   end Delete;

   ----------
   -- Find --
   ----------

   function Find (Command : String) return Rose.Capabilities.Capability is
      Position : constant Command_Maps.Cursor := Commands.Find (Command);
   begin
      if Command_Maps.Has_Element (Position) then
         return Command_Maps.Element (Position);
      else
         return Rose.Capabilities.Null_Capability;
      end if;
   end Find;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Command : String;
      Cap     : Rose.Capabilities.Capability)
   is
   begin
      Commands.Include (Command, Cap);
   end Insert;

end Command.Maps;
