package Rose.Environment is

   procedure Get_Environment_Value
     (Name  : String;
      Value : out String;
      Last  : out Natural);

   procedure Set_Environment_Value
     (Name  : String;
      Value : String);

end Rose.Environment;
