with Ada.Containers.Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Strings.Unbounded;

with WL.String_Maps;
with WL.String_Sets;
with WL.Random;

with Tropos.Reader;
with Tropos.Writer;

package body IDL.Endpoints is

   type Endpoint_Value_Record is
      record
         Endpoint : Ada.Strings.Unbounded.Unbounded_String;
         Value    : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Endpoint_Value_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Endpoint_Value_Record);

   function Less (Left, Right : Endpoint_Value_Record) return Boolean
   is (Ada.Strings.Unbounded."<"
       (Left.Endpoint, Right.Endpoint));

   package Endpoint_Sort is
     new Endpoint_Value_Lists.Generic_Sorting (Less);

   package Endpoint_Table is
     new WL.String_Maps (String);

   Table      : Endpoint_Table.Map;
   Numbers    : WL.String_Sets.Set;
   Loaded     : Boolean := False;
   Randomised : Boolean := False;

   Table_Path : Ada.Strings.Unbounded.Unbounded_String;

   function Endpoint_File_Name return String;

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
                  Numbers.Include (Item.Value);
               end loop;
            end;
         end if;
         Loaded := True;
      end if;

      if not Table.Contains (Key) then

         if not Randomised then
            WL.Random.Randomise;
            Randomised := True;
         end if;

         loop
            declare
               S : String (1 .. 12);
               Ds : constant String := "0123456789ABCDEF";
            begin
               for Ch of S loop
                  Ch := Ds (WL.Random.Random_Number (1, 16));
               end loop;

               declare
                  Value : constant String :=
                            "16#" & S (1 .. 4)
                          & "_" & S (5 .. 8)
                            & "_" & S (9 .. 12)
                          & "#";
               begin
                  if not Numbers.Contains (Value) then
                     Table.Insert (Key, Value);
                     exit;
                  end if;
               end;
            end;
         end loop;
         Save_Table;
      end if;
   end Check_Loaded;

   ------------------------
   -- Endpoint_File_Name --
   ------------------------

   function Endpoint_File_Name return String is
      use Ada.Strings.Unbounded;
   begin
      if Table_Path = Null_Unbounded_String then
         return "interfaces.txt";
      else
         return To_String (Table_Path);
      end if;
   end Endpoint_File_Name;

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
      use Ada.Strings.Unbounded;
      function "+" (S : String) return Unbounded_String
                    renames Ada.Strings.Unbounded.To_Unbounded_String;
      function "-" (S : Unbounded_String) return String
                    renames Ada.Strings.Unbounded.To_String;

      Config : Tropos.Configuration;
      List   : Endpoint_Value_Lists.List;
   begin
      for Position in Table.Iterate loop
         List.Append ((+(Endpoint_Table.Key (Position)),
                      +(Endpoint_Table.Element (Position))));
      end loop;
      Endpoint_Sort.Sort (List);
      for Item of List loop
         Config.Add (-Item.Endpoint, -Item.Value);
      end loop;
      Tropos.Writer.Write_Config (Config, Endpoint_File_Name);
   end Save_Table;

   --------------------
   -- Set_Table_Path --
   --------------------

   procedure Set_Table_Path (Path : String) is
   begin
      Table_Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
   end Set_Table_Path;

end IDL.Endpoints;
