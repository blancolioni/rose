with Ada.Directories;

with WL.String_Maps;
with WL.Random;

with Tropos.Reader;
with Tropos.Writer;

package body IDL.Endpoints is

   Endpoint_File_Name : constant String := "interfaces.txt";

   package Endpoint_Table is
     new WL.String_Maps (String);

   Table : Endpoint_Table.Map;
   Loaded : Boolean := False;

   procedure Check_Loaded (Key : String);
   procedure Save_Table;

   ------------------
   -- Check_Loaded --
   ------------------

   procedure Check_Loaded (Key : String) is
   begin
      if not Loaded then
         WL.Random.Randomise;
         if Ada.Directories.Exists (Endpoint_File_Name) then
            declare
               Config : constant Tropos.Configuration :=
                          Tropos.Reader.Read_Config (Endpoint_File_Name);
            begin
               for Item of Config loop
                  Table.Insert (Item.Config_Name, Item.Value);
               end loop;
            end;
         end if;
         Loaded := True;
      end if;

      if not Table.Contains (Key) then
         declare
            S : String (1 .. 12);
            Ds : constant String := "0123456789ABCDEF";
         begin
            for Ch of S loop
               Ch := Ds (WL.Random.Random_Number (1, 16));
            end loop;
            Table.Insert
              (Key, "16#" & S (1 .. 4)
               & "_" & S (5 .. 8)
               & "_" & S (9 .. 12)
               & "#");
         end;
         Save_Table;
      end if;
   end Check_Loaded;

   -----------------
   -- Endpoint_Id --
   -----------------

   function Endpoint_Id
     (Interface_Name  : String;
      Subprogram_Name : String)
      return String
   is
      Key : constant String := Interface_Name & "." & Subprogram_Name;
   begin
      Check_Loaded (Key);

      return Table.Element (Key);
   end Endpoint_Id;

   ----------------
   -- Save_Table --
   ----------------

   procedure Save_Table is
      Config : Tropos.Configuration;
   begin
      for Position in Table.Iterate loop
         Config.Add (Endpoint_Table.Key (Position),
                     Endpoint_Table.Element (Position));
      end loop;
      Tropos.Writer.Write_Config (Config, Endpoint_File_Name);
   end Save_Table;

end IDL.Endpoints;
