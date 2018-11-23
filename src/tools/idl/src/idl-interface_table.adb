with Ada.Characters.Handling;
with WL.String_Maps;

with IDL.Parser;

package body IDL.Interface_Table is

   package IF_Tables is
     new WL.String_Maps (IDL.Syntax.IDL_Interface, IDL.Syntax."=");

   Table : IF_Tables.Map;

   -----------
   -- Check --
   -----------

   procedure Check
     (Name : String)
   is
   begin
      if not Contains (Name) then
         declare
            File_Name : constant String :=
                          Ada.Characters.Handling.To_Lower (Name)
                        & ".idl";
            New_IF    : constant IDL.Syntax.IDL_Interface :=
                          IDL.Parser.Parse_Interface_File (File_Name);
         begin
            pragma Unreferenced (New_IF);
         end;
      end if;
   end Check;

   --------------
   -- Contains --
   --------------

   function Contains (Name : String) return Boolean is
   begin
      return Table.Contains (Name);
   end Contains;

   -------------
   -- Element --
   -------------

   function Element
     (Name : String)
      return IDL.Syntax.IDL_Interface
   is
   begin
      return Table.Element (Name);
   end Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Item : IDL.Syntax.IDL_Interface)
   is
   begin
      Table.Insert (IDL.Syntax.Get_Name (Item), Item);
   end Insert;

end IDL.Interface_Table;
