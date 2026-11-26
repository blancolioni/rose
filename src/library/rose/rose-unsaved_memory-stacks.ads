package Rose.Unsaved_Memory.Stacks is

   --  Running a procedure on a stack in unsaved memory (#1140), so that
   --  nothing it or what it calls leaves on the stack is saved by a
   --  checkpoint: a key being derived from a password, say. Its frames,
   --  and whatever the compiler spills into them, are never written to
   --  the log, however long the procedure runs.

   type Work_Procedure is
     access procedure (Argument : System.Address);
   --  Library level, so that what is called needs nothing from the frame
   --  of the caller but Argument.

   Default_Stack_Size : constant System.Storage_Elements.Storage_Count :=
                          16_384;

   procedure Call
     (Work       : not null Work_Procedure;
      Argument   : System.Address;
      Stack_Size : System.Storage_Elements.Storage_Count :=
        Default_Stack_Size);
   --  Call Work (Argument) on a new stack of at least Stack_Size bytes in
   --  unsaved memory, and zero and release the stack when Work returns.
   --  An exception that Work doesn't handle is raised again here, once
   --  the stack is released; the unwinder can't take it across the
   --  change of stack itself. Nothing guards the end of the stack, so
   --  Stack_Size has to be enough for Work. Raise Mapping_Error if the
   --  stack can't be mapped (Allocate).

end Rose.Unsaved_Memory.Stacks;
