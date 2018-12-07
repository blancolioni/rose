package Rose.Limits is

   Max_Processes : constant := 64;

   Max_Physical_Pages : constant := 2 ** 20 - 1;
   Max_Virtual_Pages : constant := 2 ** 20 - 1;

   Page_Size : constant := 2 ** 12;

end Rose.Limits;
