with System.Storage_Elements;

with Rose.Objects;
with Rose.Words;

with Rose.Invocation;

with Rose.Console_IO;

with Rose.Interfaces.Event_Listener.Server;
with Rose.Interfaces.Stream_Reader.Server;

with Rose.System_Calls;
with Rose.Server;

package body Event_Input_Stream.Server is

   use System.Storage_Elements;

   Cooked_Mode  : constant Boolean := True;
   --  In cooked mode, the stream is the console's line discipline: it
   --  echoes what is typed, acts on control characters, and a Read
   --  returns only whole lines (#397). In raw mode, every character is
   --  data, and nothing is echoed.

   EOT : constant Storage_Element := 4;
   ETX : constant Storage_Element := 3;
   BS  : constant Storage_Element := 8;
   HT  : constant Storage_Element := 9;
   LF  : constant Storage_Element := 10;
   NAK : constant Storage_Element := 21;
   ETB : constant Storage_Element := 23;
   ESC : constant Storage_Element := 27;
   DEL : constant Storage_Element := 127;

   Context         : Rose.Server.Server_Context;

   Input_Buffer : Storage_Array (1 .. 4096)
     with Alignment => 4096;
   Input_Last   : Storage_Count := 0;
   Line_Start   : Storage_Count := 0;
   --  Input_Buffer (1 .. Line_Start) are the lines that are complete and
   --  haven't been read, and Input_Buffer (Line_Start + 1 .. Input_Last)
   --  is the line being typed.

   End_Of_File  : Boolean := False;
   --  An end of file that hasn't been read yet (ADR 0011, section 3).

   type Escape_State is (Ground, Escape, Sequence);

   Escape_Input : Escape_State := Ground;
   --  Where we are in an escape sequence, which cooked mode drops.

   procedure On_Event
     (Id   : Rose.Objects.Capability_Identifier;
      Code : Rose.Words.Word);

   procedure On_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count);

   procedure Append (Element : Storage_Element);
   --  Add Element to the line being typed, and echo it in cooked mode.

   procedure Complete_Line;
   --  The line being typed is complete: a Read can return it.

   procedure Rub_Out;
   --  Remove the last character of the line being typed, and erase it
   --  from the screen.

   procedure Echo (Element : Storage_Element);

   ------------
   -- Append --
   ------------

   procedure Append (Element : Storage_Element) is
   begin
      if Input_Last < Input_Buffer'Last then
         Input_Last := Input_Last + 1;
         Input_Buffer (Input_Last) := Element;
         if Cooked_Mode then
            Echo (Element);
         end if;
      end if;

      if not Cooked_Mode
        or else Element = LF
        or else Input_Last = Input_Buffer'Last
      then
         Complete_Line;
      end if;
   end Append;

   -------------------
   -- Complete_Line --
   -------------------

   procedure Complete_Line is
   begin
      Line_Start := Input_Last;
      Rose.Server.Unblock_Endpoint
        (Context, Rose.Interfaces.Stream_Reader.Read_Endpoint);
   end Complete_Line;

   ----------
   -- Echo --
   ----------

   procedure Echo (Element : Storage_Element) is
   begin
      Rose.Console_IO.Put (Character'Val (Element));
   end Echo;

   --------------
   -- On_Event --
   --------------

   procedure On_Event
     (Id   : Rose.Objects.Capability_Identifier;
      Code : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      use type Rose.Words.Word;
      Element : constant Storage_Element :=
                  Storage_Element (Code mod 2 ** System.Storage_Unit);
   begin
      if not Cooked_Mode then
         Append (Element);
         return;
      end if;

      case Escape_Input is
         when Ground =>
            null;
         when Escape =>
            Escape_Input := (if Element = Character'Pos ('[')
                             then Sequence
                             else Ground);
            return;
         when Sequence =>

            --  An escape sequence ends with a byte in 16#40# .. 16#7E#.

            if Element in 16#40# .. 16#7E# then
               Escape_Input := Ground;
            end if;
            return;
      end case;

      case Element is
         when LF | HT | 32 .. 126 =>
            Append (Element);

         when BS | DEL =>
            Rub_Out;

         when NAK =>
            while Input_Last > Line_Start loop
               Rub_Out;
            end loop;

         when ETB =>
            while Input_Last > Line_Start
              and then Input_Buffer (Input_Last) = Character'Pos (' ')
            loop
               Rub_Out;
            end loop;
            while Input_Last > Line_Start
              and then Input_Buffer (Input_Last) /= Character'Pos (' ')
            loop
               Rub_Out;
            end loop;

         when ETX =>

            --  Discard the line. Once ADR 0006 is implemented, this also
            --  sends Interrupt to the job that is reading.

            Input_Last := Line_Start;
            Rose.Console_IO.Put ("^C");
            Rose.Console_IO.New_Line;

         when EOT =>

            --  End of file (ADR 0011, section 2). A line that is partly
            --  typed is returned without a line break, and a second
            --  Ctrl-D then ends the input.

            if Input_Last = Line_Start then
               End_Of_File := True;
            end if;
            Complete_Line;

         when ESC =>
            Escape_Input := Escape;

         when others =>

            --  Other control characters, including Ctrl-Z until ADR 0006
            --  gives it Stop, are ignored.

            null;
      end case;

      Rose.Console_IO.Flush;
   end On_Event;

   -------------
   -- On_Read --
   -------------

   procedure On_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Id);
   begin
      if Line_Start > 0 then
         declare
            Count : constant Storage_Count :=
                      Storage_Count'Min (Buffer'Length, Line_Start);
         begin
            Last := Buffer'First + Count - 1;
            Buffer (Buffer'First .. Last) := Input_Buffer (1 .. Count);
            Input_Buffer (1 .. Input_Last - Count) :=
              Input_Buffer (Count + 1 .. Input_Last);
            Input_Last := Input_Last - Count;
            Line_Start := Line_Start - Count;
         end;
      elsif End_Of_File then

         --  A Read that returns nothing is the end of the stream.

         End_Of_File := False;
         Last := Buffer'First - 1;
      else
         Rose.Server.Block_Current_Request (Context);
      end if;
   end On_Read;

   -------------
   -- Rub_Out --
   -------------

   procedure Rub_Out is
   begin
      if Input_Last > Line_Start then
         Input_Last := Input_Last - 1;
         Echo (BS);
         Echo (Character'Pos (' '));
         Echo (BS);
      end if;
   end Rub_Out;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin

      Rose.Interfaces.Stream_Reader.Server.Publish_Interface
        (Server_Context => Context,
         Read           => On_Read'Access);

      Rose.Interfaces.Event_Listener.Server.Attach_Interface
        (Server_Context => Context,
         On_Event       => On_Event'Access);

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Event_Source_Cap);
         Rose.System_Calls.Invoke_Capability (Params);

         Rose.System_Calls.Initialize_Send (Params, Params.Caps (0));
         Rose.System_Calls.Send_Cap
           (Params => Params,
            Cap    =>
              Rose.Interfaces.Event_Listener.Server.Get_On_Event_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      Rose.Server.Start_Server (Context);

   end Start_Server;

end Event_Input_Stream.Server;
