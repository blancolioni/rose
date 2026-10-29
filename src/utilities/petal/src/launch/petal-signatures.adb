package body Petal.Signatures is

   function Lower (Ch : Character) return Character
   is (if Ch in 'A' .. 'Z'
       then Character'Val (Character'Pos (Ch) + 32)
       else Ch);

   function To_Natural (Text : String) return Natural;
   --  The number at the start of Text, or zero.

   function Starts
     (Text   : String;
      Prefix : String)
      return Boolean
   is (Text'Length > Prefix'Length
       and then Text (Text'First .. Text'First + Prefix'Length - 1)
                  = Prefix);

   function After
     (Text   : String;
      Prefix : String)
      return Natural
   is (To_Natural (Text (Text'First + Prefix'Length .. Text'Last)));
   --  The number after Prefix in Text, which Starts with it.

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Text   : String;
      Result : out Signature)
   is
      Line_First : Positive;
      Current    : Natural := 0;
      --  The argument that the argument lines are about.

      procedure Line (First, Last : Positive);
      --  Take in the line Result.Text (First .. Last).

      ----------
      -- Line --
      ----------

      procedure Line (First, Last : Positive) is
         Space : Natural := 0;
      begin
         for I in First .. Last loop
            if Result.Text (I) = ' ' then
               Space := I;
               exit;
            end if;
         end loop;

         if Space = 0 then
            return;
         end if;

         declare
            Key   : constant String := Result.Text (First .. Space - 1);
            Field : constant Slice := (Space + 1, Last);
            Item  : String renames Result.Text (Space + 1 .. Last);
         begin
            if Key = "rose-command" then
               Result.Valid := Item = "1";
            elsif Key = "name" then
               Result.Name := Field;
            elsif Key = "package" then
               Result.Package_Name := Field;
            elsif Key = "title" then
               Result.Title := Field;
            elsif Key = "version" then
               Result.Version := Field;
            elsif Key = "copyright" then
               Result.Copyright := Field;
            elsif Key = "authors" then
               Result.Authors := Field;
            elsif Key = "license" then
               Result.License := Field;
            elsif Key = "help-footer" then
               Result.Help_Footer := Field;
            elsif Key = "summary" then
               Result.Summary := Field;
            elsif Key = "description" then
               Result.Description := Field;
            elsif Key = "live" then
               Result.Live := Item = "true";
            elsif Key = "stream" then
               if Result.Stream_Count < Max_Streams then
                  Result.Stream_Count := Result.Stream_Count + 1;
                  declare
                     Stream : Stream_Record renames
                       Result.Streams (Result.Stream_Count);
                  begin
                     if Starts (Item, "input ") then
                        Stream := (Input_Stream, After (Item, "input "));
                     elsif Starts (Item, "output ") then
                        Stream := (Output_Stream, After (Item, "output "));
                     elsif Starts (Item, "error ") then
                        Stream := (Error_Stream, After (Item, "error "));
                     else
                        Result.Stream_Count := Result.Stream_Count - 1;
                     end if;
                  end;
               end if;
            elsif Key = "system" or else Key = "cap" then
               if Result.Role_Cap_Count < Max_Role_Caps then
                  Result.Role_Cap_Count := Result.Role_Cap_Count + 1;
                  declare
                     Role      : Role_Cap_Record renames
                       Result.Role_Caps (Result.Role_Cap_Count);
                     Name_Last : Natural := Field.Last;
                     Number    : Positive := Field.First;
                  begin
                     --  "system <name> <index>", or "cap <role> <interface>
                     --  <index>": the name is the first word, and the index
                     --  the last.

                     for I in Field.First .. Field.Last loop
                        if Result.Text (I) = ' ' then
                           if Name_Last = Field.Last then
                              Name_Last := I - 1;
                           end if;
                           Number := I + 1;
                        end if;
                     end loop;

                     Role.Name := (Field.First, Name_Last);
                     Role.Cap_Index :=
                       To_Natural (Result.Text (Number .. Field.Last));
                     declare
                        Name : String renames
                          Result.Text (Field.First .. Name_Last);
                     begin
                        Role.Kind :=
                          (if Key = "system" and then Name = "create-endpoint"
                           then Create_Endpoint_Cap
                           elsif Key = "system" and then Name = "delete-cap"
                           then Delete_Cap
                           elsif Key = "system" and then Name = "rescind-cap"
                           then Rescind_Cap
                           elsif Key = "cap" and then Name = "Find_Command"
                           then Find_Command_Cap
                           else Other_Role_Cap);
                     end;
                  end;
               end if;
            elsif Key = "argument" then
               if Result.Argument_Count < Max_Arguments then
                  Result.Argument_Count := Result.Argument_Count + 1;
                  Current := Result.Argument_Count;
                  Result.Arguments (Current) := (Name => Field, others => <>);
               else
                  Current := 0;
               end if;
            elsif Current > 0 then
               declare
                  Argument : Argument_Record renames
                    Result.Arguments (Current);
               begin
                  if Key = "type" then
                     Argument.Of_Type :=
                       (if Item = "Boolean" then Boolean_Type
                        elsif Item = "Natural" then Natural_Type
                        elsif Item = "Positive" then Positive_Type
                        elsif Item = "String_List" then String_List_Type
                        elsif Starts (Item, "interface ")
                        then Interface_Type
                        else String_Type);
                     if Argument.Of_Type = Interface_Type then
                        Argument.Interface_Name :=
                          (Field.First + 10, Field.Last);
                     end if;
                  elsif Key = "mode" then
                     Argument.Mode :=
                       (if Item = "out" then Out_Mode else In_Mode);
                  elsif Key = "versioned" then
                     Argument.Versioned := Item = "true";
                  elsif Key = "place" then
                     if Starts (Item, "word ") then
                        Argument.Place := Word_Place;
                        Argument.Index := After (Item, "word ");
                     elsif Starts (Item, "item ") then
                        Argument.Place := Item_Place;
                        Argument.Index := After (Item, "item ");
                     elsif Starts (Item, "cap ") then
                        Argument.Place := Cap_Place;
                        Argument.Index := After (Item, "cap ");
                     elsif Starts (Item, "list ") then
                        --  list I word W
                        Argument.Place := List_Place;
                        Argument.Index := After (Item, "list ");
                        for I in reverse Item'Range loop
                           if Item (I) = ' ' then
                              Argument.Count_Word :=
                                To_Natural (Item (I + 1 .. Item'Last));
                              exit;
                           end if;
                        end loop;
                     end if;
                  elsif Key = "default" then
                     Argument.Has_Default := True;
                     Argument.Default := Field;
                  elsif Key = "short" then
                     Argument.Short_Name := Item (Item'First);
                  elsif Key = "short-value" then
                     Argument.Has_Short_Value := True;
                     Argument.Short_Value := Item = "True";
                  elsif Key = "help" then
                     Argument.Help := Field;
                  end if;
               end;
            end if;
         end;
      end Line;

   begin
      --  Result can hold the signature of an earlier command, so a field
      --  that this command's metadata leaves out must be cleared.

      Result.Valid := False;
      Result.Name := (others => <>);
      Result.Package_Name := (others => <>);
      Result.Title := (others => <>);
      Result.Version := (others => <>);
      Result.Copyright := (others => <>);
      Result.Authors := (others => <>);
      Result.License := (others => <>);
      Result.Help_Footer := (others => <>);
      Result.Summary := (others => <>);
      Result.Description := (others => <>);
      Result.Live := False;
      Result.Length := Natural'Min (Text'Length, Max_Text);
      Result.Text (1 .. Result.Length) :=
        Text (Text'First .. Text'First + Result.Length - 1);
      Result.Argument_Count := 0;
      Result.Stream_Count := 0;
      Result.Role_Cap_Count := 0;

      Line_First := 1;
      for I in 1 .. Result.Length loop
         if Result.Text (I) = Character'Val (10) then
            if I > Line_First then
               Line (Line_First, I - 1);
            end if;
            Line_First := I + 1;
         end if;
      end loop;
   end Parse;

   ---------------
   -- Same_Name --
   ---------------

   function Same_Name
     (Item     : Signature;
      Argument : Argument_Record;
      Name     : String)
      return Boolean
   is
      Argument_Name : String renames
        Item.Text (Argument.Name.First .. Argument.Name.Last);
   begin
      if Name'Length /= Argument_Name'Length then
         return False;
      end if;

      for I in Name'Range loop
         declare
            Left  : constant Character :=
              (if Name (I) = '-' then '_' else Lower (Name (I)));
            Right : constant Character :=
              Lower (Argument_Name (I - Name'First + Argument_Name'First));
         begin
            if Left /= Right then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Same_Name;

   ----------------
   -- To_Natural --
   ----------------

   function To_Natural (Text : String) return Natural is
      Result : Natural := 0;
   begin
      for Ch of Text loop
         exit when Ch not in '0' .. '9';
         Result := Result * 10 + Character'Pos (Ch) - 48;
      end loop;
      return Result;
   end To_Natural;

   -----------
   -- Value --
   -----------

   function Value
     (Item  : Signature;
      Field : Slice)
      return String
   is
      Result : String (1 .. Field.Last - Field.First + 1);
      Last   : Natural := 0;
      Index  : Positive := Field.First;
   begin
      while Index <= Field.Last loop
         Last := Last + 1;
         if Item.Text (Index) = '\' and then Index < Field.Last then
            Index := Index + 1;
            Result (Last) :=
              (if Item.Text (Index) = 'n' then Character'Val (10)
               else Item.Text (Index));
         else
            Result (Last) := Item.Text (Index);
         end if;
         Index := Index + 1;
      end loop;
      return Result (1 .. Last);
   end Value;

end Petal.Signatures;
