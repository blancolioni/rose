package Tests.Secondary_Stack is

   function Secondary_Stack_Test return Boolean;
   --  Results on the secondary stack past its first 10 KB chunk (#172):
   --  the stack grows by allocating chunks from the heap, and reuses
   --  them once they are released.

end Tests.Secondary_Stack;
