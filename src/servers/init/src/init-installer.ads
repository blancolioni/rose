with Rose.Capabilities;
with Rose.Words;

with Init.Calls;

package Init.Installer is

   function Install_Exec_Library
     (Bank_Cap        : Rose.Capabilities.Capability;
      Reserve_Cap     : Rose.Capabilities.Capability;
      Launch_Cap      : Rose.Capabilities.Capability;
      Launch_Command_Cap : Rose.Capabilities.Capability;
      Trace_Cap       : Rose.Capabilities.Capability;
      Cap_Stream      : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Binary_Stream   : Rose.Capabilities.Capability;
      Binary_Length   : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  Launch_Cap and Launch_Command_Cap are the ELF server's Launch and
   --  Launch_Command. Trace_Cap is for exec's trace events.

   function Install_Command_Library
     (Bank_Cap        : Rose.Capabilities.Capability;
      Reserve_Cap     : Rose.Capabilities.Capability;
      Launch_Cap      : Rose.Capabilities.Capability;
      Cap_Stream      : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Binary_Stream   : Rose.Capabilities.Capability;
      Binary_Length   : Rose.Words.Word)
      return Rose.Capabilities.Capability;

   function Install_Executable
     (Install_Cap   : Rose.Capabilities.Capability;
      Cap_Stream    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Binary_Length : Rose.Words.Word;
      Name          : String;
      Is_Driver     : Boolean;
      Extra_Caps    : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability;
   --  Is_Driver is True if the program is installed from the drivers
   --  directory, which allows it more caps (Grant_Caps).

   function Update_Executable
     (Update_Cap    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Name          : String;
      Install_Caps  : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability;
   --  Install the image Binary_Stream as the new version of the program
   --  Name, with Update_Cap, exec's Update, and Install_Caps as its
   --  install caps. Its Launch interface, or null if it failed.

end Init.Installer;
