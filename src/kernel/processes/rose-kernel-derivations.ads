with Rose.Objects;

package Rose.Kernel.Derivations
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   pragma Unevaluated_Use_Of_Old (Allow);

   --  The derivation records of the kernel (ADR 0001).  A capability that
   --  was derived names a record, which names the record that it was
   --  derived from, so a capability is valid only if no record from its
   --  own up to the root has been revoked.
   --
   --  A record is used while a capability names it (a slot) or another
   --  record does (a child), and is free when nothing does.  Releasing the
   --  last reference to a record frees it, which releases its parent in
   --  turn.  Nothing here knows of a process or a capability: the slots
   --  are counted, and Rose.Kernel.Processes says which they are.
   --
   --  What is proved of it is in the contracts, and in the invariant of
   --  the table: a record that is free is all zeros, a record that is used
   --  is one deeper than its parent, which is used, and has at most
   --  Max_Depth levels, so there are no cycles, and the children that a
   --  record counts are the used records that name it.

   use type Rose.Objects.Derivation_Id;

   subtype Record_Id is Rose.Objects.Derivation_Id;
   --  0 is no record: a capability that was not derived, or the parent of
   --  a record that was derived from one.

   subtype Record_Number is Record_Id range 1 .. Record_Id'Last;

   Max_Records : constant := 4095;

   Max_Depth : constant := 8;
   --  The most levels of derivation, so that a check is at most this many
   --  reads.

   Max_Slots : constant := 2 ** 24;
   --  More than the capabilities that all processes can have.

   type Depth_Count is range 0 .. Max_Depth;
   type Slot_Count is range 0 .. Max_Slots;

   type Table is private;
   --  A table that is all zeros is empty, as it starts out.

   function Is_Used
     (T  : Table;
      Id : Record_Id)
      return Boolean;
   --  Something names Id.  This is False for 0.

   function Parent
     (T  : Table;
      Id : Record_Id)
      return Record_Id;
   --  The record that Id was derived from, or 0, for a record that is
   --  used.

   function Depth
     (T  : Table;
      Id : Record_Id)
      return Depth_Count;
   --  The number of records from Id up to the root, counting both, for a
   --  record that is used.  It is 0 for 0.

   function Slots
     (T  : Table;
      Id : Record_Id)
      return Slot_Count;
   --  The number of capabilities that name Id.

   function Is_Revoked
     (T  : Table;
      Id : Record_Id)
      return Boolean;

   function Is_Valid_Chain
     (T  : Table;
      Id : Record_Id)
      return Boolean
   with Ghost,
        Pre => Id = 0 or else Is_Used (T, Id),
        Subprogram_Variant => (Decreases => Depth (T, Id));
   --  Neither Id nor any record above it has been revoked.  This is true
   --  for 0.

   function Is_Below
     (T  : Table;
      J  : Record_Id;
      Id : Record_Number)
      return Boolean
   with Ghost,
        Pre => J = 0 or else Is_Used (T, J),
        Subprogram_Variant => (Decreases => Depth (T, J));
   --  J is Id, or is derived from it, however far down.

   function Is_Valid
     (T  : Table;
      Id : Record_Id)
      return Boolean
   with Pre  => Id = 0 or else Is_Used (T, Id),
        Post => Is_Valid'Result = Is_Valid_Chain (T, Id);
   --  The check that a call through a derived capability makes: at most
   --  Max_Depth reads, and no allocation.

   procedure Derive
     (T         : in out Table;
      Parent_Id : Record_Id;
      Id        : out Record_Id)
   with Pre  => (Parent_Id = 0 or else Is_Used (T, Parent_Id))
                and then Depth (T, Parent_Id) < Max_Depth,
        Post => (if Id = 0
                 then (for all J in Record_Number => Is_Used (T'Old, J))
                      and then T = T'Old
                 else Id in Record_Number
                      and then not Is_Used (T'Old, Id)
                      and then Is_Used (T, Id)
                      and then Parent (T, Id) = Parent_Id
                      and then Depth (T, Id) = Depth (T'Old, Parent_Id) + 1
                      and then Slots (T, Id) = 1
                      and then not Is_Revoked (T, Id)
                      and then (for all J in Record_Number =>
                                  (if J /= Id
                                   then Is_Used (T, J) = Is_Used (T'Old, J)
                                        and then Parent (T, J)
                                                 = Parent (T'Old, J)
                                        and then Depth (T, J)
                                                 = Depth (T'Old, J)
                                        and then Is_Revoked (T, J)
                                                 = Is_Revoked (T'Old, J)
                                        and then Slots (T, J)
                                                 = Slots (T'Old, J))));
   --  Make a record, one level below Parent_Id, with one slot that names
   --  it, which is the capability that is being derived.  Id is the lowest
   --  record that was free, or 0 if there is none.

   procedure Restore_Record
     (T         : in out Table;
      Id        : Record_Number;
      Parent_Id : Record_Id;
      Revoked   : Boolean)
   with Pre  => not Is_Used (T, Id)
                and then (Parent_Id = 0 or else Is_Used (T, Parent_Id))
                and then Depth (T, Parent_Id) < Max_Depth,
        Post => Is_Used (T, Id)
                and then Parent (T, Id) = Parent_Id
                and then Depth (T, Id) = Depth (T'Old, Parent_Id) + 1
                and then Slots (T, Id) = 1
                and then Is_Revoked (T, Id) = Revoked
                and then (for all J in Record_Number =>
                            (if J /= Id
                             then Is_Used (T, J) = Is_Used (T'Old, J)
                                  and then Parent (T, J)
                                           = Parent (T'Old, J)
                                  and then Depth (T, J)
                                           = Depth (T'Old, J)
                                  and then Is_Revoked (T, J)
                                           = Is_Revoked (T'Old, J)
                                  and then Slots (T, J)
                                           = Slots (T'Old, J)));
   --  Make the record Id, as Derive does, but at a number that is given,
   --  and revoked if it was: for a restore from a checkpoint, which puts
   --  the records back at the numbers that capabilities name.  Its one
   --  slot is a hold for the restore, which the capabilities that name it
   --  add to as they are restored, and which Remove_Slot then drops.

   procedure Add_Slot
     (T  : in out Table;
      Id : Record_Number)
   with Pre  => Is_Used (T, Id) and then Slots (T, Id) < Max_Slots,
        Post => Slots (T, Id) = Slots (T'Old, Id) + 1
                and then (for all J in Record_Number =>
                            Is_Used (T, J) = Is_Used (T'Old, J)
                            and then Parent (T, J) = Parent (T'Old, J)
                            and then Depth (T, J) = Depth (T'Old, J)
                            and then Is_Revoked (T, J)
                                     = Is_Revoked (T'Old, J)
                            and then (if J /= Id
                                      then Slots (T, J)
                                           = Slots (T'Old, J)));
   --  A capability that names Id has been copied.

   procedure Revoke
     (T  : in out Table;
      Id : Record_Number)
   with Pre  => Is_Used (T, Id),
        Post => Is_Revoked (T, Id)
                and then (for all J in Record_Number =>
                            Is_Used (T, J) = Is_Used (T'Old, J)
                            and then Parent (T, J) = Parent (T'Old, J)
                            and then Depth (T, J) = Depth (T'Old, J)
                            and then Slots (T, J) = Slots (T'Old, J)
                            and then (if J /= Id
                                      then Is_Revoked (T, J)
                                           = Is_Revoked (T'Old, J)))
                and then (for all J in Record_Number =>
                            (if Is_Used (T, J)
                             then Is_Valid_Chain (T, J)
                                  = (Is_Valid_Chain (T'Old, J)
                                     and then
                                       not Is_Below (T'Old, J, Id))));
   --  Every capability that names Id, or a record below it, is no longer
   --  valid, and no other capability is affected.  Nothing is freed.

   procedure Remove_Slot
     (T  : in out Table;
      Id : Record_Number)
   with Pre  => Is_Used (T, Id) and then Slots (T, Id) > 0,
        Post => Slots (T, Id) = Slots (T'Old, Id) - 1
                and then (for all J in Record_Number =>
                            (if Is_Used (T, J) then Is_Used (T'Old, J))
                            and then Parent (T, J) = Parent (T'Old, J)
                            and then Depth (T, J) = Depth (T'Old, J)
                            and then Is_Revoked (T, J)
                                     = Is_Revoked (T'Old, J)
                            and then (if J /= Id
                                      then Slots (T, J)
                                           = Slots (T'Old, J)));
   --  A capability that names Id has been deleted.  If that was the last
   --  reference to Id, the record is free, and the record that it was
   --  derived from loses a child, which may free that in turn.  Nothing is
   --  used that was not.

private

   subtype Child_Count is Natural range 0 .. Max_Records;

   type Record_Entry is
      record
         Parent   : Record_Id    := 0;
         Depth    : Depth_Count  := 0;
         Revoked  : Boolean      := False;
         Slots    : Slot_Count   := 0;
         Children : Child_Count  := 0;
      end record;

   Empty_Entry : constant Record_Entry := (others => <>);

   function Used (E : Record_Entry) return Boolean
   is (E.Slots > 0 or else E.Children > 0);

   type Record_Array is array (Record_Number) of Record_Entry;

   type Table is
      record
         Records : Record_Array := (others => Empty_Entry);
      end record
   with Type_Invariant => Consistent (Table);

   function Count_Children
     (T    : Table;
      Id   : Record_Id;
      Upto : Child_Count)
      return Natural
   is (if Upto = 0
       then 0
       else Count_Children (T, Id, Upto - 1)
            + (if Used (T.Records (Record_Id (Upto)))
                  and then T.Records (Record_Id (Upto)).Parent = Id
               then 1 else 0))
   with Ghost,
        Post => Count_Children'Result <= Upto
                and then
                  ((Count_Children'Result = 0)
                   = (for all J in 1 .. Upto =>
                        not (Used (T.Records (Record_Id (J)))
                             and then T.Records (Record_Id (J)).Parent
                                      = Id))),
        Subprogram_Variant => (Decreases => Upto);
   --  The used records numbered 1 to Upto that name Id.

   function Linked (T : Table) return Boolean
   is (for all Id in Record_Number =>
         (if Used (T.Records (Id))
          then T.Records (Id).Depth >= 1
               and then
                 (if T.Records (Id).Parent = 0
                  then T.Records (Id).Depth = 1
                  else T.Records (Id).Parent /= Id
                       and then Used (T.Records (T.Records (Id).Parent))
                       and then Integer (T.Records (Id).Depth)
                                = Integer
                                    (T.Records
                                       (T.Records (Id).Parent).Depth) + 1)))
   with Ghost;
   --  A record that is used is one level below its parent, which is used.
   --  What a record that is free holds does not matter: it is made again by
   --  Derive.

   function Counted (T : Table) return Boolean
   is (for all Id in Record_Number =>
         T.Records (Id).Children = Count_Children (T, Id, Max_Records))
   with Ghost;
   --  The children that a record counts are the used records that name it.

   function Consistent (T : Table) return Boolean
   is (Linked (T) and then Counted (T))
   with Ghost;

   function Is_Used
     (T  : Table;
      Id : Record_Id)
      return Boolean
   is (Id /= 0 and then Used (T.Records (Id)));

   function Parent
     (T  : Table;
      Id : Record_Id)
      return Record_Id
   is (if Id = 0 then 0 else T.Records (Id).Parent);

   function Depth
     (T  : Table;
      Id : Record_Id)
      return Depth_Count
   is (if Id = 0 then 0 else T.Records (Id).Depth);

   function Slots
     (T  : Table;
      Id : Record_Id)
      return Slot_Count
   is (if Id = 0 then 0 else T.Records (Id).Slots);

   function Is_Revoked
     (T  : Table;
      Id : Record_Id)
      return Boolean
   is (Id /= 0 and then T.Records (Id).Revoked);

   function Is_Below
     (T  : Table;
      J  : Record_Id;
      Id : Record_Number)
      return Boolean
   is (J /= 0
       and then (J = Id or else Is_Below (T, T.Records (J).Parent, Id)));

   function Is_Valid_Chain
     (T  : Table;
      Id : Record_Id)
      return Boolean
   is (Id = 0
       or else (not T.Records (Id).Revoked
                and then Is_Valid_Chain (T, T.Records (Id).Parent)));

end Rose.Kernel.Derivations;
