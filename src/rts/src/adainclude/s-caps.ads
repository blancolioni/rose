with Rose.Capabilities;

package System.Caps is

   function Get_Environment_Cap
     (Index : Positive)
      return Rose.Capabilities.Capability;

   function Standard_Input  return Rose.Capabilities.Capability;
   function Standard_Output return Rose.Capabilities.Capability;
   function Standard_Error  return Rose.Capabilities.Capability;

end System.Caps;
