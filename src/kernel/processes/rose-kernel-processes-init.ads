with Rose.Environment_Pages;
with Rose.Kernel.Modules;

package Rose.Kernel.Processes.Init is

   procedure Init_Process_Table;

   function Load_Boot_Module
     (Priority    : Process_Priority;
      Module      : Rose.Kernel.Modules.Module_Index;
      Environment : access Rose.Environment_Pages.Environment_Page)
      return Rose.Objects.Process_Id;

end Rose.Kernel.Processes.Init;
