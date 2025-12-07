package body Configure.Caps is

   ------------
   -- Append --
   ------------

   procedure Append
     (This : in out Cap_Config_List;
      Cap  : Cap_Config'Class)
   is
   begin
      This.List.Append (Cap_Config (Cap));
   end Append;

   ------------
   -- Create --
   ------------

   function Create
     (Name    : String;
      Field_1 : String := "";
      Field_2 : String := "";
      Field_3 : String := "";
      Field_4 : String := "")
      return Cap_Config
   is
      Cap : Cap_Config := Cap_Config'
        (Key_Length => Name'Length,
         Key        => Name,
         Fields     => <>);

      procedure Append (Field : String);

      ------------
      -- Append --
      ------------

      procedure Append (Field : String) is
      begin
         if Field /= "" then
            Cap.Fields.Append (Field);
         end if;
      end Append;

   begin
      Append (Field_1);
      Append (Field_2);
      Append (Field_3);
      Append (Field_4);
      return Cap;
   end Create;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This    : Cap_Config_List;
      Process : not null access
        procedure (Cap : Cap_Config))
   is
   begin
      for Cap of This.List loop
         Process (Cap);
      end loop;
   end Iterate;

end Configure.Caps;
