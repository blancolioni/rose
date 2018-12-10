with Rose.Capabilities;

package Rose.Environment is

   Standard_Create_Cap : constant Rose.Capabilities.Capability := 1;
   Standard_Delete_Cap : constant Rose.Capabilities.Capability := 2;

   procedure Get_Environment_Value
     (Name  : String;
      Value : out String;
      Last  : out Natural);

   procedure Set_Environment_Value
     (Name  : String;
      Value : String);

end Rose.Environment;
