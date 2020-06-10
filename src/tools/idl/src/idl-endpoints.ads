package IDL.Endpoints is

   function Endpoint_Id
     (Interface_Name  : String;
      Subprogram_Name : String)
      return String;

   procedure Set_Table_Path (Path : String);

end IDL.Endpoints;
