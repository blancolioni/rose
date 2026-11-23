with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Init.Grants;

package body Init.Command_Line is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   Header_Size : constant := 8;
   --  A tag starts with its type and its size, each four bytes. The
   --  command line's text follows, ending with a zero byte.

   Tag : Storage_Array (1 .. 512) := (others => 0);
   --  Not on the stack, which is a page in init (#1004).

   Tag_Cap : Rose.Capabilities.Capability :=
               Rose.Capabilities.Null_Capability;

   ------------------
   -- Has_Argument --
   ------------------

   function Has_Argument (Name : String) return Boolean is
      use type Rose.Words.Word;
      Params : aliased Rose.Invocation.Invocation_Record;
      Size   : Storage_Count;
      Start  : Storage_Offset;
   begin
      if Tag_Cap = Rose.Capabilities.Null_Capability then
         Tag_Cap := Init.Grants.Boot_Tag (1);
         if Tag_Cap = Rose.Capabilities.Null_Capability then
            return False;
         end if;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Tag_Cap);
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

end Init.Command_Line;
