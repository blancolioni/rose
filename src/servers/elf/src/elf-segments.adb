package body Elf.Segments
  with SPARK_Mode
is

   use type Elf.Format.Program_Header_Type;

   function To_Segment (Header : Elf.Format.Program_Header) return Segment
   is (File_Offset     => Header.P_Offset,
       File_Size       => Header.P_FileSz,
       Virtual_Address => Header.P_VAddr,
       Memory_Size     => Header.P_MemSz,
       Readable        => (Header.P_Flags and 4) = 4,
       Writable        => (Header.P_Flags and 2) = 2,
       Executable      => (Header.P_Flags and 1) = 1);

   --------------
   -- Describe --
   --------------

   procedure Describe
     (Headers    : Program_Header_Array;
      Image_Size : Rose.Words.Word_64;
      Limit      : Rose.Words.Word_64;
      List       : out Segment_List;
      Success    : out Boolean)
   is
   begin
      List := (Count => 0, Items => (others => (others => <>)));
      Success := False;

      for I in Headers'Range loop
         pragma Loop_Invariant (List.Count <= I - Headers'First);
         pragma Loop_Invariant (Valid (List, Image_Size, Limit));

         if Headers (I).P_Type = Elf.Format.PT_LOAD then
            declare
               Item : constant Segment := To_Segment (Headers (I));
            begin
               if not Valid (Item, Image_Size, Limit) then
                  return;
               end if;

               for J in 1 .. List.Count loop
                  pragma Loop_Invariant
                    (for all K in 1 .. J - 1 =>
                       Disjoint (List.Items (K), Item));
                  pragma Loop_Invariant (Valid (List, Image_Size, Limit));

                  if not Disjoint (List.Items (J), Item) then
                     return;
                  end if;
               end loop;

               pragma Assert
                 (for all K in 1 .. List.Count =>
                    Disjoint (List.Items (K), Item));

               List.Count := List.Count + 1;
               List.Items (List.Count) := Item;

               pragma Assert
                 (for all J in 1 .. List.Count - 1 =>
                    (for all I in 1 .. J - 1 =>
                       Disjoint (List.Items (I), List.Items (J))));
               pragma Assert
                 (for all I in 1 .. List.Count - 1 =>
                    Disjoint (List.Items (I), List.Items (List.Count)));
               pragma Assert (Valid (List, Image_Size, Limit));
            end;
         end if;
      end loop;

      pragma Assert (Valid (List, Image_Size, Limit));
      Success := True;
   end Describe;

end Elf.Segments;
