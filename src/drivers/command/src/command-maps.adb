package body Command.Maps is

   Max_Command_Length : constant := 32;

   subtype Command_String is String (1 .. Max_Command_Length);

   type Command_Record is
      record
         Name_Length : Natural := 0;
         Name        : Command_String := (others => ' ');
         Cap         : Rose.Capabilities.Capability :=
                         Rose.Capabilities.Null_Capability;
      end record;

   Max_Commands       : constant := 100;
   type Command_Count is range 0 .. Max_Commands;
   subtype Command_Index is Command_Count range 1 .. Command_Count'Last;

   Current_Count      : Command_Count := 0;
   Empty_Slot         : Command_Count := 0;
   Command_Array      : array (Command_Index) of Command_Record;

   function Match (Index : Command_Index;
                   Name  : String)
                   return Boolean;

   ------------
   -- Delete --
   ------------

   procedure Delete (Command : String) is
   begin
      for I in 1 .. Current_Count loop
         if Match (I, Command) then
            Command_Array (I) := (others => <>);
            if Empty_Slot = 0 or else I < Empty_Slot then
               Empty_Slot := I;
            end if;
            exit;
         end if;
      end loop;
   end Delete;

   ----------
   -- Find --
   ----------

   function Find (Command : String) return Rose.Capabilities.Capability is
   begin
      for I in 1 .. Current_Count loop
         if Match (I, Command) then
            return Command_Array (I).Cap;
         end if;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Find;

   ------------
   -- Insert --
   ------------

   procedure Insert (Command : String;
                     Cap     : Rose.Capabilities.Capability)
   is
      Rec : Command_Record :=
              Command_Record'
                (Name_Length => Command'Length,
                 Name        => <>,
                 Cap         => Cap);
   begin
      Rec.Name (1 .. Rec.Name_Length) := Command;
      if Empty_Slot > 0 then
         Command_Array (Empty_Slot) := Rec;
         Empty_Slot := 0;
      else
         Current_Count := Current_Count + 1;
         Command_Array (Current_Count) := Rec;
      end if;
   end Insert;

   -----------
   -- Match --
   -----------

   function Match (Index : Command_Index;
                   Name  : String)
                   return Boolean
   is
      Rec : Command_Record renames Command_Array (Index);
   begin
      return Rec.Name_Length > 0
        and then Rec.Name_Length = Name'Length
        and then Rec.Name (1 .. Rec.Name_Length) = Name;
   end Match;

end Command.Maps;
