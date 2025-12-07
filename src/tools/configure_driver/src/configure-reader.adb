with Ada.Text_IO;                      use Ada.Text_IO;

package body Configure.Reader is

   function To_Cap_Config (Line : String) return Caps.Cap_Config;

   ----------
   -- Read --
   ----------

   function Read
     (Path : String)
      return Configure.Caps.Cap_Config_List
   is
      File : File_Type;
      List : Configure.Caps.Cap_Config_List;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Full_Line : constant String := Get_Line (File);
            Cap       : constant Configure.Caps.Cap_Config :=
                          To_Cap_Config (Full_Line);
         begin
            if Cap.Name /= "" then
               List.Append (Cap);
            end if;
         end;
      end loop;
      Close (File);
      return List;
   end Read;

   function To_Cap_Config (Line : String) return Caps.Cap_Config is
      Name_Start  : Natural := 0;
      Name_End    : Natural := 0;
      Field_Start : array (1 .. 4) of Natural := (others => 0);
      Field_End   : array (1 .. 4) of Natural := (others => 0);
      Field_Count : Natural := 0;
      In_Name     : Boolean := False;
      In_Field    : Boolean := False;
      Have_Name   : Boolean := False;
      CR          : constant Character := Character'Val (13);

      function Return_Cap return Caps.Cap_Config;

      function Return_Cap return Caps.Cap_Config is
         Name : constant String :=
                  (if Have_Name then Line (Name_Start .. Name_End)
                   else "");
         Field_1 : constant String :=
                     (if Field_Start (1) > 0
                      then Line (Field_Start (1) .. Field_End (1))
                      else "");
         Field_2 : constant String :=
                     (if Field_Start (2) > 0
                      then Line (Field_Start (2) .. Field_End (2))
                      else "");

      begin
         return Caps.Create (Name, Field_1, Field_2);
      end Return_Cap;

      Extended_Line : constant String := Line & ' ';
   begin
      for Index in Extended_Line'Range loop
         declare
            Ch : constant Character := Extended_Line (Index);
         begin
            case Ch is
               when '#' | CR =>
                  exit;
               when ' ' =>
                  if In_Name then
                     Name_End := Index - 1;
                     In_Name := False;
                     Have_Name := True;
                  elsif In_Field then
                     Field_End (Field_Count) := Index - 1;
                     In_Field := False;
                  end if;
               when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' =>
                  if not Have_Name then
                     if not In_Name then
                        In_Name := True;
                        Name_Start := Index;
                     end if;
                  else
                     if not In_Field then
                        In_Field := True;
                        Field_Count := Field_Count + 1;
                        Field_Start (Field_Count) := Index;
                     end if;
                  end if;
               when others =>
                  raise Constraint_Error with
                    "bad character: " & Ch;
            end case;
         end;
      end loop;

      return Return_Cap;
   end To_Cap_Config;

end Configure.Reader;
