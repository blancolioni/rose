package Rose.Version is

   pragma Pure (Rose.Version);

   Version_Major   : constant := 0;
   Version_Minor   : constant := 2;
   Version_Release : constant := 0;

   Version_String  : constant String := "0.2.0";
   Version_Name    : constant String := "putney";

   Full_Name       : constant String :=
     "Rose Version " & Version_String & " (" & Version_Name & ")";

end Rose.Version;
