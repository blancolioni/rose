with Rose.Environment_Pages;
with Rose.Kernel.Modules;

package Rose.Kernel.Processes.Init is

   procedure Init_Process_Table;

   function Load_Boot_Module
     (Priority    : Process_Priority;
      Module      : Rose.Kernel.Modules.Module_Index;
      Oid         : Rose.Objects.Boot_Process_Object_Id;
      Environment : access Rose.Environment_Pages.Environment_Page)
      return Rose.Kernel.Processes.Process_Id;
   --  Load boot module Module as a new process with object id Oid, which
   --  no live process may have.

end Rose.Kernel.Processes.Init;
