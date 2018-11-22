with Ada.Characters.Handling;

package body IDL.Identifiers is

   -----------------
   -- To_Ada_Name --
   -----------------

   function To_Ada_Name (Item : String) return String is
      use Ada.Characters.Handling;
      Result : String  := Item;
      First  : Boolean := True;
   begin
      for I in Result'Range loop
         if Is_Letter (Result (I)) then
            if First then
               Result (I) := To_Upper (Result (I));
               First := False;
            else
               Result (I) := To_Lower (Result (I));
            end if;
         elsif Result (I) = '-' then
            Result (I) := '.';
            First := True;
         elsif Result (I) = '.' or else Result (I) = '_' then
            First := True;
         end if;
      end loop;
      return Result;
   end To_Ada_Name;

   -----------------------
   -- To_Body_File_Name --
   -----------------------

   function To_Body_File_Name (Base_Name : String) return String is
      Result : constant String := To_File_Name (Base_Name);
   begin
      return Result & ".adb";
   end To_Body_File_Name;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name (Subprogram_Name : String) return String is
      use Ada.Characters.Handling;
      Result : String := Subprogram_Name;
   begin
      for I in Result'Range loop
         if Is_Upper (Result (I)) then
            Result (I) := To_Lower (Result (I));
         elsif Result (I) = '.' then
            Result (I) := '-';
         end if;
      end loop;
      return Result;
   end To_File_Name;

   -----------------------
   -- To_Spec_File_Name --
   -----------------------

   function To_Spec_File_Name (Base_Name : String) return String is
      Result : constant String := To_File_Name (Base_Name);
   begin
      return Result & ".ads";
   end To_Spec_File_Name;

end IDL.Identifiers;
