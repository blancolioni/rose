with Rose.Objects;
with Rose.Words;

package Init.Installer is

   procedure Install_Exec_Library
     (Create_Cap      : Rose.Capabilities.Capability;
      Storage_Cap     : Rose.Capabilities.Capability;
      Reserve_Cap     : Rose.Capabilities.Capability;
      Launch_Cap      : Rose.Capabilities.Capability;
      Cap_Stream      : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Binary_Stream   : Rose.Capabilities.Capability;
      Binary_Length   : Rose.Words.Word);

   function Install_Executable
     (Create_Cap    : Rose.Capabilities.Capability;
      Cap_Stream    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Binary_Length : Rose.Words.Word)
      return Rose.Objects.Object_Id;

end Init.Installer;
