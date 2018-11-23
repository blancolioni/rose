with IDL.Syntax;

package IDL.Interface_Table is

   function Contains (Name : String) return Boolean;
   function Element (Name : String) return IDL.Syntax.IDL_Interface
     with Pre => Contains (Name);

   procedure Insert
     (Item : IDL.Syntax.IDL_Interface)
     with Pre => not Contains (IDL.Syntax.Get_Name (Item));

   procedure Check
     (Name : String);

end IDL.Interface_Table;
