package IDL.Identifiers is

   function To_Ada_Name (Item : String) return String;
   function To_File_Name (Subprogram_Name : String) return String;
   function To_Spec_File_Name (Base_Name : String) return String;
   function To_Body_File_Name (Base_Name : String) return String;

end IDL.Identifiers;
