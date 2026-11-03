package body Rose.Kernel.Derivations
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);

   --  What changing one record does to the children that are counted.  It
   --  is stated in the fields of the records and not through Is_Used,
   --  because a table in the middle of a change need not be consistent.

   function Term
     (T : Table;
      K : Record_Number;
      P : Record_Id)
      return Natural
   is (if Used (T.Records (K)) and then T.Records (K).Parent = P
       then 1 else 0)
   with Ghost;
   --  1 if record K is a child of P that is counted.

   function Contribution
     (T : Table;
      K : Record_Number;
      P : Record_Id;
      N : Natural)
      return Natural
   is (if Natural (K) <= N then Term (T, K, P) else 0)
   with Ghost;
   --  What record K adds to the children of P that are counted up to N.

   function Same_Links
     (Old_T : Table;
      New_T : Table;
      K     : Record_Number)
      return Boolean
   is (for all J in Record_Number =>
         (if J /= K
          then Used (New_T.Records (J)) = Used (Old_T.Records (J))
               and then
                 (if Used (Old_T.Records (J))
                  then New_T.Records (J).Parent = Old_T.Records (J).Parent)))
   with Ghost;
   --  Every record but K is used if it was, and names the record that it
   --  did.

   procedure Lemma_Count_Update
     (Old_T : Table;
      New_T : Table;
      K     : Record_Number;
      P     : Record_Id;
      N     : Child_Count)
   with Ghost,
        Pre  => Same_Links (Old_T, New_T, K),
        Post => Integer (Count_Children (New_T, P, N))
                = Integer (Count_Children (Old_T, P, N))
                  - Integer (Contribution (Old_T, K, P, N))
                  + Integer (Contribution (New_T, K, P, N)),
        Subprogram_Variant => (Decreases => N);

   procedure Lemma_Count_Update
     (Old_T : Table;
      New_T : Table;
      K     : Record_Number;
      P     : Record_Id;
      N     : Child_Count)
   is
   begin
      if N > 0 then
         Lemma_Count_Update (Old_T, New_T, K, P, N - 1);
      end if;
   end Lemma_Count_Update;

   procedure Lemma_Count_Update_All
     (Old_T : Table;
      New_T : Table;
      K     : Record_Number)
   with Ghost,
        Pre  => Same_Links (Old_T, New_T, K),
        Post => (for all P in Record_Number =>
                   Integer (Count_Children (New_T, P, Max_Records))
                   = Integer (Count_Children (Old_T, P, Max_Records))
                     - Integer (Contribution (Old_T, K, P, Max_Records))
                     + Integer (Contribution (New_T, K, P, Max_Records)));

   procedure Lemma_Count_Update_All
     (Old_T : Table;
      New_T : Table;
      K     : Record_Number)
   is
   begin
      for P in Record_Number loop
         Lemma_Count_Update (Old_T, New_T, K, P, Max_Records);

         pragma Loop_Invariant
           (for all Q in Record_Number =>
              (if Q <= P
               then Integer (Count_Children (New_T, Q, Max_Records))
                    = Integer (Count_Children (Old_T, Q, Max_Records))
                      - Integer (Contribution (Old_T, K, Q, Max_Records))
                      + Integer (Contribution (New_T, K, Q, Max_Records))));
      end loop;
   end Lemma_Count_Update_All;

   function No_Children
     (T  : Table;
      Id : Record_Id)
      return Boolean
   is (for all K in Record_Number =>
         not (Used (T.Records (K)) and then T.Records (K).Parent = Id))
   with Ghost;
   --  No record that is used names Id.

   procedure Lemma_Count_Zero
     (T  : Table;
      Id : Record_Id;
      N  : Child_Count;
      J  : Record_Number)
   with Ghost,
        Pre  => Count_Children (T, Id, N) = 0 and then Natural (J) <= N,
        Post => not (Used (T.Records (J)) and then T.Records (J).Parent = Id),
        Subprogram_Variant => (Decreases => N);

   procedure Lemma_Count_Zero
     (T  : Table;
      Id : Record_Id;
      N  : Child_Count;
      J  : Record_Number)
   is
   begin
      if Natural (J) < N then
         Lemma_Count_Zero (T, Id, N - 1, J);
      end if;
   end Lemma_Count_Zero;

   procedure Lemma_No_Children
     (T  : Table;
      Id : Record_Id)
   with Ghost,
        Pre  => Count_Children (T, Id, Max_Records) = 0,
        Post => No_Children (T, Id);

   procedure Lemma_No_Children
     (T  : Table;
      Id : Record_Id)
   is
   begin
      for K in Record_Number loop
         Lemma_Count_Zero (T, Id, Max_Records, K);
         pragma Loop_Invariant
           (for all Q in Record_Number =>
              (if Q <= K
               then not (Used (T.Records (Q))
                         and then T.Records (Q).Parent = Id)));
      end loop;
   end Lemma_No_Children;

   function Pending_Counted
     (T : Table;
      P : Record_Id)
      return Boolean
   is (for all I in Record_Number =>
         T.Records (I).Children
         = Count_Children (T, I, Max_Records) + (if I = P then 1 else 0))
   with Ghost;
   --  Counted, except that P, if it is a record, counts a child that is
   --  free.

   function Mid
     (T   : Table;
      Cur : Record_Number)
      return Boolean
   is (Linked (T)
       and then not Used (T.Records (Cur))
       and then T.Records (Cur).Parent /= Cur
       and then Pending_Counted (T, T.Records (Cur).Parent)
       and then
         (if T.Records (Cur).Parent = 0
          then T.Records (Cur).Depth = 1
          else Used (T.Records (T.Records (Cur).Parent))
               and then Integer (T.Records (Cur).Depth)
                        = Integer
                            (T.Records (T.Records (Cur).Parent).Depth) + 1))
   with Ghost;
   --  Cur has just been freed, and the record that it was derived from
   --  has not yet been told.

   function Settled
     (T   : Table;
      Cur : Record_Id)
      return Boolean
   is (if Cur = 0 or else Used (T.Records (Cur))
       then Consistent (T)
       else Mid (T, Cur))
   with Ghost;
   --  Either the table is consistent, or Cur is the record that has just
   --  been freed.

   procedure Lemma_Revoke_Scope
     (Old_T : Table;
      New_T : Table;
      Id    : Record_Number;
      J     : Record_Id)
   with Ghost,
        Pre  => Consistent (Old_T)
                and then Consistent (New_T)
                and then (for all K in Record_Number =>
                            New_T.Records (K).Parent
                            = Old_T.Records (K).Parent
                            and then New_T.Records (K).Depth
                                     = Old_T.Records (K).Depth
                            and then New_T.Records (K).Slots
                                     = Old_T.Records (K).Slots
                            and then New_T.Records (K).Children
                                     = Old_T.Records (K).Children
                            and then (if K /= Id
                                      then New_T.Records (K).Revoked
                                           = Old_T.Records (K).Revoked))
                and then New_T.Records (Id).Revoked
                and then (J = 0 or else Is_Used (Old_T, J)),
        Post => Is_Valid_Chain (New_T, J)
                = (Is_Valid_Chain (Old_T, J)
                   and then not Is_Below (Old_T, J, Id)),
        Subprogram_Variant => (Decreases => Depth (Old_T, J));

   procedure Lemma_Revoke_Scope
     (Old_T : Table;
      New_T : Table;
      Id    : Record_Number;
      J     : Record_Id)
   is
   begin
      if J /= 0 then
         Lemma_Revoke_Scope (Old_T, New_T, Id, Old_T.Records (J).Parent);
      end if;
   end Lemma_Revoke_Scope;

   procedure Lemma_Revoke_Scope_All
     (Old_T : Table;
      New_T : Table;
      Id    : Record_Number)
   with Ghost,
        Pre  => Consistent (Old_T)
                and then Consistent (New_T)
                and then (for all K in Record_Number =>
                            New_T.Records (K).Parent
                            = Old_T.Records (K).Parent
                            and then New_T.Records (K).Depth
                                     = Old_T.Records (K).Depth
                            and then New_T.Records (K).Slots
                                     = Old_T.Records (K).Slots
                            and then New_T.Records (K).Children
                                     = Old_T.Records (K).Children
                            and then (if K /= Id
                                      then New_T.Records (K).Revoked
                                           = Old_T.Records (K).Revoked))
                and then New_T.Records (Id).Revoked,
        Post => (for all Q in Record_Number =>
                   (if Is_Used (Old_T, Q)
                    then Is_Valid_Chain (New_T, Q)
                         = (Is_Valid_Chain (Old_T, Q)
                            and then not Is_Below (Old_T, Q, Id))));

   procedure Lemma_Revoke_Scope_All
     (Old_T : Table;
      New_T : Table;
      Id    : Record_Number)
   is
   begin
      for J in Record_Number loop
         if Is_Used (Old_T, J) then
            Lemma_Revoke_Scope (Old_T, New_T, Id, J);
         end if;

         pragma Loop_Invariant
           (for all Q in Record_Number =>
              (if Q <= J and then Is_Used (Old_T, Q)
               then Is_Valid_Chain (New_T, Q)
                    = (Is_Valid_Chain (Old_T, Q)
                       and then not Is_Below (Old_T, Q, Id))));
      end loop;
   end Lemma_Revoke_Scope_All;

   --------------
   -- Add_Slot --
   --------------

   procedure Add_Slot
     (T  : in out Table;
      Id : Record_Number)
   is
      Old_T : constant Table := T with Ghost;
   begin
      T.Records (Id).Slots := T.Records (Id).Slots + 1;
      Lemma_Count_Update_All (Old_T, T, Id);
   end Add_Slot;

   ------------
   -- Derive --
   ------------

   procedure Derive
     (T         : in out Table;
      Parent_Id : Record_Id;
      Id        : out Record_Id)
   is
   begin
      Id := 0;

      for N in 1 .. Max_Records loop
         pragma Loop_Invariant (Id = 0);
         pragma Loop_Invariant
           (for all K in Record_Number =>
              (if Natural (K) < N then Used (T.Records (K))));

         if not Used (T.Records (Record_Id (N))) then
            Id := Record_Id (N);
            exit;
         end if;
      end loop;

      pragma Assert
        (if Id = 0
         then (for all K in Record_Number => Used (T.Records (K))));

      if Id /= 0 then
         declare
            Old_T : constant Table := T with Ghost;
            Level : constant Depth_Count := Depth (T, Parent_Id) + 1;
         begin
            T.Records (Id) :=
              (Parent   => Parent_Id,
               Depth    => Level,
               Revoked  => False,
               Slots    => 1,
               Children => 0);

            if Parent_Id /= 0 then
               --  What the parent counts, with this record, is at most
               --  all of them, so there is room for it.
               Lemma_Count_Update_All (Old_T, T, Id);

               T.Records (Parent_Id).Children :=
                 T.Records (Parent_Id).Children + 1;
            end if;

            Lemma_Count_Update_All (Old_T, T, Id);

            pragma Assert (Linked (T));
            pragma Assert (Counted (T));
         end;
      end if;
   end Derive;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (T  : Table;
      Id : Record_Id)
      return Boolean
   is
      Cur : Record_Id := Id;
   begin
      while Cur /= 0 loop
         pragma Loop_Invariant (Cur = 0 or else Is_Used (T, Cur));
         pragma Loop_Invariant
           (Is_Valid_Chain (T, Id) = Is_Valid_Chain (T, Cur));
         pragma Loop_Variant (Decreases => Depth (T, Cur));

         if T.Records (Cur).Revoked then
            return False;
         end if;

         Cur := T.Records (Cur).Parent;
      end loop;

      return True;
   end Is_Valid;

   -----------------
   -- Remove_Slot --
   -----------------

   procedure Remove_Slot
     (T  : in out Table;
      Id : Record_Number)
   is
      Old_T : constant Table := T with Ghost;
      Cur   : Record_Id := Id;
   begin
      T.Records (Id).Slots := T.Records (Id).Slots - 1;
      Lemma_Count_Update_All (Old_T, T, Id);

      pragma Assert (Counted (Old_T));
      pragma Assert
        (if not Used (T.Records (Id))
         then Count_Children (Old_T, Id, Max_Records) = 0);
      if not Used (T.Records (Id)) then
         Lemma_No_Children (Old_T, Id);
         pragma Assert (No_Children (Old_T, Id));
      end if;

      pragma Assert (Linked (T));
      pragma Assert
        (if not Used (T.Records (Id))
         then Pending_Counted (T, T.Records (Id).Parent));
      pragma Assert
        (if not Used (T.Records (Id))
         then Mid (T, Id));
      pragma Assert (Settled (T, Id));

      while Cur /= 0 and then not Used (T.Records (Cur)) loop
         pragma Loop_Invariant (Settled (T, Cur));
         pragma Loop_Invariant (Cur /= 0 and then not Used (T.Records (Cur)));
         pragma Loop_Invariant
           (for all J in Record_Number =>
              T.Records (J).Parent = Old_T.Records (J).Parent
              and then T.Records (J).Depth = Old_T.Records (J).Depth
              and then T.Records (J).Revoked = Old_T.Records (J).Revoked
              and then (if J /= Id
                        then T.Records (J).Slots = Old_T.Records (J).Slots)
              and then (if Used (T.Records (J))
                        then Used (Old_T.Records (J))));
         pragma Loop_Variant (Decreases => T.Records (Cur).Depth);

         declare
            Before : constant Table := T with Ghost;
            Above  : constant Record_Id := T.Records (Cur).Parent;
         begin
            if Above /= 0 then
               T.Records (Above).Children :=
                 T.Records (Above).Children - 1;
               Lemma_Count_Update_All (Before, T, Above);

               pragma Assert (Linked (Before));
               pragma Assert (Used (Before.Records (Above)));

               if Used (T.Records (Above)) then
                  pragma Assert (Linked (T));
                  pragma Assert
                    (for all I in Record_Number =>
                       Term (Before, Above, I) = Term (T, Above, I));
                  pragma Assert (Counted (T));
               else
                  pragma Assert
                    (for all I in Record_Number => Term (T, Above, I) = 0);
                  pragma Assert
                    (for all I in Record_Number =>
                       Term (Before, Above, I)
                       = (if Before.Records (Above).Parent = I
                          then 1 else 0));
                  pragma Assert
                    (for all I in Record_Number =>
                       Count_Children (T, I, Max_Records)
                       = Count_Children (Before, I, Max_Records)
                         - (if Before.Records (Above).Parent = I
                            then 1 else 0));
                  pragma Assert (Count_Children (T, Above, Max_Records) = 0);
                  Lemma_No_Children (T, Above);
                  pragma Assert (Linked (T));
                  pragma Assert
                    (Pending_Counted (T, T.Records (Above).Parent));
                  pragma Assert (Mid (T, Above));
               end if;
            end if;

            Cur := Above;
         end;
      end loop;
   end Remove_Slot;

   ------------
   -- Revoke --
   ------------

   procedure Revoke
     (T  : in out Table;
      Id : Record_Number)
   is
      Old_T : constant Table := T with Ghost;
   begin
      T.Records (Id).Revoked := True;
      Lemma_Count_Update_All (Old_T, T, Id);

      pragma Assert (Consistent (T));

      Lemma_Revoke_Scope_All (Old_T, T, Id);
   end Revoke;

end Rose.Kernel.Derivations;
