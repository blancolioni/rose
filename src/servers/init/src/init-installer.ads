with Rose.Words;

with Init.Calls;

package Init.Installer is

   function Install_Exec_Library
     (Create_Cap      : Rose.Capabilities.Capability;
      Storage_Cap     : Rose.Capabilities.Capability;
      Reserve_Cap     : Rose.Capabilities.Capability;
      Launch_Cap      : Rose.Capabilities.Capability;
      Cap_Stream      : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Binary_Stream   : Rose.Capabilities.Capability;
      Binary_Length   : Rose.Words.Word)
      return Rose.Capabilities.Capability;

   function Install_Command_Library
     (Create_Cap      : Rose.Capabilities.Capability;
      Storage_Cap     : Rose.Capabilities.Capability;
      Reserve_Cap     : Rose.Capabilities.Capability;
      Launch_Cap      : Rose.Capabilities.Capability;
      Cap_Stream      : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Binary_Stream   : Rose.Capabilities.Capability;
      Binary_Length   : Rose.Words.Word)
      return Rose.Capabilities.Capability;

   function Install_Executable
     (Create_Cap    : Rose.Capabilities.Capability;
      Install_Cap   : Rose.Capabilities.Capability;
      Cap_Stream    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Binary_Length : Rose.Words.Word;
      Extra_Caps    : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability;

end Init.Installer;
