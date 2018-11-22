with IDL.Procs;
with IDL.Syntax;

package IDL.Parser is

   Parse_Error : exception;

   function Parse_Interface_File
     (Path : String)
      return IDL.Syntax.IDL_Interface;

   function Parse_Petal_File
     (Path : String)
      return IDL.Procs.IDL_Procedure;

end IDL.Parser;
