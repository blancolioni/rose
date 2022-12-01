package body Rose.Environment_Pages is

   procedure Find
     (Page  : Environment_Page;
      Name  : String;
      Index : out Natural;
      First : out Natural;
      Last  : out Natural);

   -----------
   -- Clear --
   -----------

   procedure Clear (Page : in out Environment_Page) is
   begin
      Page := (others => <>);
   end Clear;

   ------------------------------
   -- Delete_Environment_Value --
   ------------------------------

   procedure Delete_Environment_Value
     (Page  : in out Environment_Page;
      Name  : String)
   is
      use Rose.Words;
      Index                   : Natural;
      Value_First, Value_Last : Natural;
      Text_Offset             : Positive;
   begin
      Find (Page, Name, Index, Value_First, Value_Last);

      if Index = 0 then
         return;
      end if;

      Text_Offset := (Value_Last - Value_First + 1) + 1 + Name'Length;

      declare
         Text_Low : constant Natural := Value_Last + 1;
         Text_High : constant Natural := Natural (Page.Text_Length);
      begin
         for I in Text_Low .. Text_High loop
            Page.Text (I - Text_Offset) := Page.Text (I);
         end loop;
         Page.Text_Length := Page.Text_Length - Word_32 (Text_Offset);
      end;

      declare
         Index_Low : constant Values_Per_Page_Range :=
                       Values_Per_Page_Range (Index + 1);
         Index_High : constant Values_Per_Page_Range :=
                        Values_Per_Page_Range (Page.Value_Count);
      begin
         for I in Index_Low .. Index_High loop
            Page.Indices (I - 1) :=
              (Page.Indices (I).Start - Word_16 (Text_Offset),
               Page.Indices (I).Length);
         end loop;
      end;
      Page.Value_Count := Page.Value_Count - 1;
   end Delete_Environment_Value;

   ----------
   -- Find --
   ----------

   procedure Find
     (Page  : Environment_Page;
      Name  : String;
      Index : out Natural;
      First : out Natural;
      Last  : out Natural)
   is
      use Rose.Words;
   begin

      Index := 0;
      First := 0;
      Last := 0;

      for I in 1 .. Values_Per_Page_Range (Page.Value_Count) loop
         declare
            Index : Value_Index_Record renames
                      Page.Indices (I);
         begin
            if Index.Start > 0 and then Index.Length > 0 then
               declare
                  Start       : constant Positive := Positive (Index.Start);
                  Length      : constant Positive := Positive (Index.Length);
                  Name_Index  : Natural := Name'First - 1;
               begin
                  for J in Start .. Start + Length - 1 loop
                     Name_Index := Name_Index + 1;
                     if Page.Text (J) = '=' then
                        if Name_Index = Name'Last + 1 then
                           First := J + 1;
                           Last  := Start + Length - 1;
                        end if;
                        exit;
                     elsif Name_Index > Name'Last
                       or else Name (Name_Index) /= Page.Text (J)
                     then
                        exit;
                     end if;
                  end loop;
               end;
            end if;
         end;

         if First > 0 then
            Index := Positive (I);
            exit;
         end if;

      end loop;
   end Find;

   ---------------------------
   -- Get_Environment_Value --
   ---------------------------

   procedure Get_Environment_Value
     (Page  : Environment_Page;
      Name  : String;
      Value : out String;
      Last  : out Natural)
   is
      Index                   : Natural;
      Value_First, Value_Last : Natural;
   begin
      Find (Page, Name, Index, Value_First, Value_Last);

      Last := 0;
      if Index > 0 then
         for J in Value_First .. Value_Last loop
            exit when Last >= Value'Last;
            Last := Last + 1;
            Value (Last) := Page.Text (J);
         end loop;
      end if;

   end Get_Environment_Value;

   ------------------------------
   -- Insert_Environment_Value --
   ------------------------------

   procedure Insert_Environment_Value
     (Page     : in out Environment_Page;
      Name     : String;
      Value    : String;
      Inserted : out Boolean)
   is
      use Rose.Words;
      Index : Natural;
      Value_First, Value_Last : Natural;
      New_Start, New_Length   : Positive;
   begin
      Find (Page, Name, Index, Value_First, Value_Last);

      if Index > 0 then
         Delete_Environment_Value (Page, Name);
      end if;

      if Natural (Page.Value_Count) >= Max_Values_Per_Page then
         Inserted := False;
         return;
      end if;

      Page.Value_Count := Page.Value_Count + 1;
      New_Length := Name'Length + Value'Length + 1;

      if Page.Value_Count = 1 then
         New_Start := 1;
      else
         declare
            Previous_Rec : Value_Index_Record renames
                             Page.Indices
                               (Values_Per_Page_Range (Page.Value_Count) - 1);
         begin
            New_Start :=
              Positive (Previous_Rec.Start + Previous_Rec.Length);
         end;
      end if;

      if New_Start + New_Length > Page.Text'Last then
         Inserted := False;
         return;
      end if;

      declare
         Index_Rec    : Value_Index_Record renames
                          Page.Indices
                            (Values_Per_Page_Range (Page.Value_Count));
         To_Index     : Natural := New_Start - 1;
      begin

         Index_Rec := (Word_16 (New_Start), Word_16 (New_Length));
         for Ch of Name loop
            To_Index := To_Index + 1;
            Page.Text (To_Index) := Ch;
         end loop;

         To_Index := To_Index + 1;
         Page.Text (To_Index) := '=';

         for Ch of Value loop
            To_Index := To_Index + 1;
            Page.Text (To_Index) := Ch;
         end loop;
         Page.Text_Length := Word_32 (To_Index);
      end;

      Inserted := True;

   end Insert_Environment_Value;

   ---------------------------
   -- Set_Environment_Value --
   ---------------------------

   procedure Set_Environment_Value
     (Page     : in out Environment_Page;
      Name     : String;
      Value    : String)
   is
      Inserted : Boolean;
   begin
      Insert_Environment_Value (Page, Name, Value, Inserted);
   end Set_Environment_Value;

end Rose.Environment_Pages;
