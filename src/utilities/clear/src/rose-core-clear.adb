with System.Storage_Elements;

procedure Rose.Core.Clear
  (Console : Rose.Interfaces.Terminal.Client.Terminal_Client)
is
   use System.Storage_Elements;

   ESC : constant Storage_Element := 27;

   --  ESC [ 2 J clears the screen, and ESC [ H moves the cursor to its
   --  top left corner (interfaces/terminal.idl).

   Clear_Screen : constant Storage_Array :=
                    (ESC, Character'Pos ('['), Character'Pos ('2'),
                     Character'Pos ('J'),
                     ESC, Character'Pos ('['), Character'Pos ('H'));
begin
   Rose.Interfaces.Terminal.Client.Write (Console, Clear_Screen);
end Rose.Core.Clear;
