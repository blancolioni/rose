procedure Rose.Core.Clear
  (Console : Rose.Interfaces.Terminal.Client.Terminal_Client)
is
begin
   --  Clearing the screen also moves the cursor to its top left corner
   --  (interfaces/terminal.idl, #734).

   Rose.Interfaces.Terminal.Client.Clear (Console);
end Rose.Core.Clear;
