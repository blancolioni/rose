package body Rose.Command_Signatures.Help is

   --------------
   -- Put_Help --
   --------------

   procedure Put_Help (Item : Signature) is
      Name : constant String := Value (Item, Item.Name);
   begin
      Put ("Usage: " & Name);
      if (for some I in 1 .. Item.Argument_Count =>
            Item.Arguments (I).Short_Name /= ' ')
      then
         Put (" [OPTION]...");
      end if;
      for Argument of Item.Arguments (1 .. Item.Argument_Count) loop
         if Argument.Short_Name = ' ' then
            declare
               Upper : String := Value (Item, Argument.Name);
            begin
               for Ch of Upper loop
                  if Ch in 'a' .. 'z' then
                     Ch := Character'Val (Character'Pos (Ch) - 32);
                  end if;
               end loop;
               if Argument.Of_Type = String_List_Type then
                  Put (" [" & Upper & "]...");
               elsif Argument.Has_Default
                 or else Is_In_Directory (Item, Argument)
               then
                  Put (" [" & Upper & "]");
               else
                  Put (" " & Upper);
               end if;
            end;
         end if;
      end loop;
      Put_Line ("");

      Put_Line (Value (Item, Item.Summary));
      Put_Line ("");

      --  The options. A String_List argument takes the remaining words,
      --  so it is only in the usage line.

      for Argument of Item.Arguments (1 .. Item.Argument_Count) loop
         if Argument.Of_Type /= String_List_Type then
            declare
               Long : String := Value (Item, Argument.Name);
            begin
               for Ch of Long loop
                  if Ch = '_' then
                     Ch := '-';
                  elsif Ch in 'A' .. 'Z' then
                     Ch := Character'Val (Character'Pos (Ch) + 32);
                  end if;
               end loop;
               Put ("  "
                    & (if Argument.Short_Name = ' ' then "    "
                       else "-" & Argument.Short_Name & ", ")
                    & "--"
                    & (if Argument.Of_Type = Boolean_Type then "[no-]" else "")
                    & Long);
               if Argument.Of_Type /= Boolean_Type then
                  Put ("=VALUE");
               end if;
               Put_Line ("");
               Put_Line ("        " & Value (Item, Argument.Help));
            end;
         end if;
      end loop;
      Put_Line ("      --help     display this help and exit");
      Put_Line ("      --version  output version information and exit");

      if Item.Description.Last >= Item.Description.First then
         Put_Line ("");
         Put_Line (Value (Item, Item.Description));
      end if;

      if Item.Help_Footer.Last >= Item.Help_Footer.First then
         Put_Line ("");
         Put_Line (Value (Item, Item.Help_Footer));
      end if;
   end Put_Help;

   -----------------
   -- Put_Version --
   -----------------

   procedure Put_Version (Item : Signature) is
   begin
      Put_Line (Value (Item, Item.Name)
                & " (" & Value (Item, Item.Title) & ") "
                & Value (Item, Item.Version));
      if Item.Copyright.Last >= Item.Copyright.First then
         Put_Line (Value (Item, Item.Copyright));
      end if;
      if Item.License.Last >= Item.License.First then
         Put_Line (Value (Item, Item.License));
      end if;
      if Item.Authors.Last >= Item.Authors.First then
         Put_Line ("");
         Put_Line ("Written by " & Value (Item, Item.Authors) & ".");
      end if;
   end Put_Version;

end Rose.Command_Signatures.Help;
