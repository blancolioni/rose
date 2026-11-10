with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

package body Restore.Command_Line is

   use System.Storage_Elements;

   Header_Size : constant := 8;
   --  A tag starts with its type and its size, each four bytes.  The
   --  command line's text follows, ending with a zero byte.

   Tag : Storage_Array (1 .. 512) := (others => 0);

   ------------------
   -- Has_Argument --
   ------------------

   function Has_Argument (Name : String) return Boolean is
      use type Rose.Capabilities.Capability;
      use type Rose.Words.Word;
      Params : aliased Rose.Invocation.Invocation_Record;
      Size   : Storage_Count;
      Start  : Storage_Offset;
   begin
      if Command_Line_Cap = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Command_Line_Cap);
      Rose.System_Calls.Send_Buffer
        (Params, Tag'Length, Tag'Address, Writable => True);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else Params.Data (0) <= Header_Size
      then
         return False;
      end if;

      Size :=
        Storage_Count'Min (Tag'Length, Storage_Count (Params.Data (0)));

      --  Compare each word of the text with Name.

      Start := Tag'First + Header_Size;
      while Start <= Size loop
         declare
            Finish : Storage_Offset := Start;
         begin
            while Finish <= Size
              and then Tag (Finish) /= 0
              and then Tag (Finish) /= Character'Pos (' ')
            loop
               Finish := Finish + 1;
            end loop;

            if Natural (Finish - Start) = Name'Length then
               declare
                  Same : Boolean := True;
               begin
                  for I in Name'Range loop
                     if Tag (Start + Storage_Offset (I - Name'First))
                       /= Character'Pos (Name (I))
                     then
                        Same := False;
                     end if;
                  end loop;
                  if Same then
                     return True;
                  end if;
               end;
            end if;

            exit when Finish > Size or else Tag (Finish) = 0;
            Start := Finish + 1;
         end;
      end loop;

      return False;
   end Has_Argument;

end Restore.Command_Line;
