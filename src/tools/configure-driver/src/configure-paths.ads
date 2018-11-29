package Configure.Paths is

   Config_Path : constant String :=
     "/home/fraser/kiln/rose-trunk/src/tools/configure-driver/config";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Configure.Paths;
