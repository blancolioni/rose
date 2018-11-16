with Rose.Words;                       use Rose.Words;

package Rose.Devices.Console is

   Console_Start : constant := 16#C00B_8000# + 2 * 80;

   Num_Lines   : constant := 24;
   Num_Columns : constant := 80;

   type Console_Memory_Array is
     array (0 .. Num_Lines * Num_Columns - 1) of Word_16;
   for Console_Memory_Array'Component_Size use 16;

end Rose.Devices.Console;
