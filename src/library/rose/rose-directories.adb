package body Rose.Directories is

   Root    : Directory_Entry_Type;
   Current : Directory_Entry_Type;

   procedure Name_To_Directory_Entry
     (Name            : String;
      Directory_Entry : out Directory_Entry_Type);

   procedure Search_Next
     (Search : in out Search_Type);

   function Match
     (Name    : String;
      Pattern : String)
      return Boolean;

   ----------------
   -- End_Search --
   ----------------

   procedure End_Search (Search : in out Search_Type) is
   begin
      Search.Finished := True;
   end End_Search;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
      Item : Directory_Entry_Type;
   begin
      Name_To_Directory_Entry (Name, Item);
      return Item.Valid;
   end Exists;

   ---------------
   -- Full_Name --
   ---------------

   procedure Full_Name
     (Directory_Entry : Directory_Entry_Type;
      Name            : out String;
      Last            : out Natural)
   is
   begin
      Last := 0;

      if not Directory_Entry.Valid then
         return;
      end if;

      for Ch of
        Directory_Entry.Entry_Name
          (1 .. Directory_Entry.Entry_Name_Last)
      loop
         exit when Last >= Name'Last;
         Last := Last + 1;
         Name (Last) := Ch;
      end loop;

      if Directory_Entry.Entry_Index > 0 then
         if Last < Name'Last then
            Last := Last + 1;
            Name (Last) := '/';
         end if;

         if Last < Name'Last then
            Last := Last + 1;
            Directory_Entry_Name
              (Item   => Directory_Entry.Containing_Directory,
               Index  => Directory_Entry.Entry_Index,
               Result => Name (Last .. Name'Last),
               Last   => Last);
         end if;
      end if;

   end Full_Name;

   --------------------
   -- Get_Next_Entry --
   --------------------

   procedure Get_Next_Entry
     (Search          : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type)
   is
   begin
      Directory_Entry := Search.Directory;
   end Get_Next_Entry;

   ----------
   -- Kind --
   ----------

   function Kind (Name : String) return File_Kind is
      Item : Directory_Entry_Type;
   begin
      Name_To_Directory_Entry (Name, Item);
      return Kind (Item);
   end Kind;

   ----------
   -- Kind --
   ----------

   function Kind
     (Directory_Entry : Directory_Entry_Type)
      return File_Kind
   is
   begin
      return Directory_Entry.Kind;
   end Kind;

   -----------
   -- Match --
   -----------

   function Match
     (Name    : String;
      Pattern : String)
      return Boolean
   is
      Pat_Index  : Positive := Pattern'First;
   begin
      for Ch of Name loop
         if Pat_Index > Pattern'Last then
            return False;
         end if;

         if Pattern (Pat_Index) = Ch then
            Pat_Index := Pat_Index + 1;
         elsif Pattern (Pat_Index) = '*' then
            if Pat_Index < Pattern'Last
              and then Pattern (Pat_Index + 1) = Ch
            then
               Pat_Index := Pat_Index + 2;
            end if;
         else
            return False;
         end if;
      end loop;

      return Pat_Index >= Pattern'Last;
   end Match;

   ------------------
   -- More_Entries --
   ------------------

   function More_Entries (Search : Search_Type) return Boolean is
   begin
      return not Search.Finished;
   end More_Entries;

   -----------------------------
   -- Name_To_Directory_Entry --
   -----------------------------

   procedure Name_To_Directory_Entry
     (Name            : String;
      Directory_Entry : out Directory_Entry_Type)
   is
      Start   : Positive := Name'First;
      At_File : Boolean := False;
   begin
      if Name (Start) = '/' then
         Directory_Entry := Root;
         Start := Start + 1;
      else
         Directory_Entry := Current;
      end if;

      for I in Start .. Name'Last + 1 loop
         if I > Name'Last or else Name (I) = '/' then
            if I > Start + 1 then

               if At_File then
                  Directory_Entry.Valid := False;
                  return;
               end if;

               declare
                  Next_Index : constant Natural :=
                                 Find_Entry
                                   (Directory_Entry.Containing_Directory,
                                    Name (Start .. I - 1));
               begin
                  if Next_Index = 0 then
                     Directory_Entry.Valid := False;
                     return;
                  end if;

                  case Directory_Entry_Kind
                    (Directory_Entry.Containing_Directory, Next_Index)
                  is
                     when Rose.Interfaces.Directory.Ordinary_File =>
                        Directory_Entry.Entry_Index := Next_Index;
                        Directory_Entry.Entry_Name
                          (1 .. I - Start) := Name (Start .. I - 1);
                        Directory_Entry.Entry_Name_Last := I - Start;
                        Directory_Entry.Valid := True;
                        Directory_Entry.Kind := Ordinary_File;
                        At_File := True;
                     when Rose.Interfaces.Directory.Directory =>
                        Directory_Entry.Containing_Directory :=
                          Get_Directory
                            (Directory_Entry.Containing_Directory,
                             Next_Index);
                        Directory_Entry.Entry_Index := 0;
                        Directory_Entry.Kind := Directory;
                        Directory_Entry.Valid := True;
                     when Rose.Interfaces.Directory.Special_File =>
                        Directory_Entry.Valid := False;
                        return;
                  end case;

                  Start := I + 1;
               end;
            end if;
         end if;

      end loop;


   end Name_To_Directory_Entry;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : out Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Name : String)
   is
      Item : Directory_Entry_Type;
   begin
      Name_To_Directory_Entry (Name, Item);
      Open (File, Item);
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (File            : out Rose.Interfaces.Stream_Reader.Client
      .Stream_Reader_Client;
      Directory_Entry : Directory_Entry_Type)
   is
   begin
      File := Read_File (Directory_Entry.Containing_Directory,
                         Directory_Entry.Entry_Index);
   end Open;

   ---------------------------
   -- Open_Root_File_System --
   ---------------------------

   procedure Open_Root_File_System
     (Client : Rose.Interfaces.File_System.Client.File_System_Client)
   is
      Top : constant Directory_Client :=
              Rose.Interfaces.File_System.Client.Root_Directory (Client);
   begin
      Root := (Top, (others => ' '), 0, 0, True, Directory);
      Current := Root;
   end Open_Root_File_System;

   ------------
   -- Search --
   ------------

   procedure Search
     (Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True);
      Process   : not null access procedure
        (Directory_Entry : Directory_Entry_Type))
   is
      Search_State : Search_Type;
   begin
      Start_Search (Search_State, Directory, Pattern, Filter);
      while More_Entries (Search_State) loop
         declare
            Item : Directory_Entry_Type;
         begin
            Get_Next_Entry (Search_State, Item);
            Process (Item);
         end;
      end loop;
   end Search;

   procedure Search_Next
     (Search : in out Search_Type)
   is
      Index : Positive :=
                Search.Directory.Entry_Index + 1;
   begin
      if Search.Finished then
         return;
      end if;

      declare
         Dir : Directory_Client renames Search.Directory.Containing_Directory;
         Max : constant Natural :=
                 Directory_Entry_Count (Dir);
      begin
         while Index <= Max loop
            declare
               Kind : constant File_Kind :=
                        File_Kind'Val
                          (Rose.Interfaces.Directory.File_Kind'Pos
                             (Directory_Entry_Kind (Dir, Index)));
            begin
               if Search.Filter (Kind) then
                  declare
                     Entry_Name : Entry_Name_Type;
                     Name_Last  : Natural;
                  begin
                     Directory_Entry_Name (Dir, Index, Entry_Name, Name_Last);
                     if Match (Entry_Name (1 .. Name_Last),
                               Search.Pattern (1 .. Search.Pattern_Length))
                     then
                        Search.Finished := False;
                        return;
                     end if;
                  end;
               end if;
            end;
            Index := Index + 1;
         end loop;
      end;
      Search.Finished := True;

   end Search_Next;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory (Directory : String) is
   begin
      Name_To_Directory_Entry (Directory, Current);
   end Set_Directory;

   -----------------
   -- Simple_Name --
   -----------------

   procedure Simple_Name
     (Directory_Entry : Directory_Entry_Type;
      Name            : out String;
      Last            : out Natural)
   is
      Start : Natural := Directory_Entry.Entry_Name_Last;
   begin
      while Start > 0 loop
         exit when Directory_Entry.Entry_Name (Start) = '/';
         Start := Start - 1;
      end loop;
      Last := Name'First - 1;
      for I in Start + 1 .. Directory_Entry.Entry_Name_Last loop
         exit when Last >= Name'Last;
         Last := Last + 1;
         Name (Last) := Directory_Entry.Entry_Name (I);
      end loop;
   end Simple_Name;

   ----------
   -- Size --
   ----------

   function Size (Name : String) return File_Size is
      Item : Directory_Entry_Type;
   begin
      Name_To_Directory_Entry (Name, Item);
      return Size (Item);
   end Size;

   ----------
   -- Size --
   ----------

   function Size
     (Directory_Entry : Directory_Entry_Type)
      return File_Size
   is
   begin
      if Directory_Entry.Valid then
         return File_Size
           (Directory_Entry_Size
              (Directory_Entry.Containing_Directory,
               Directory_Entry.Entry_Index));
      else
         return 0;
      end if;
   end Size;

   ------------------
   -- Start_Search --
   ------------------

   procedure Start_Search
     (Search    : in out Search_Type;
      Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True))
   is
   begin
      Name_To_Directory_Entry (Directory, Search.Directory);
      Search.Finished := False;
      Search.Pattern (1 .. Pattern'Length) := Pattern;
      Search.Pattern_Length := Pattern'Length;
      Search.Filter := Filter;
      Search_Next (Search);
   end Start_Search;

end Rose.Directories;
