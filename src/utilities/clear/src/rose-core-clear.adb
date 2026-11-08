procedure Rose.Core.Clear
  (Console : in out Rose.Interfaces.Terminal.Terminal_Object'Class)
is
begin
   --  Clearing the screen also moves the cursor to its top left corner
   --  (interfaces/terminal.idl, #734).

   Console.Clear;
end Rose.Core.Clear;
