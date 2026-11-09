package Rose.Version is

   pragma Pure (Rose.Version);

   Version_Major   : constant := 0;
   Version_Minor   : constant := 4;
   Version_Release : constant := 0;

   Version_String  : constant String := "0.4.0-dev";
   Version_Name    : constant String := "genki";

   Full_Name       : constant String :=
     "Rose Version " & Version_String & " (" & Version_Name & ")";

end Rose.Version;
