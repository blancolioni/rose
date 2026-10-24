private with System.Storage_Elements;

package Rose.Command_Line is

   --  Types for the arguments of commands (ADR 0002). The command's main
   --  receives them through Rose.Command_Line.Launch.

   type String_List is private;
   --  The value of a String_List argument: the words it takes. A list
   --  refers to the launch invocation it came from (see
   --  Rose.Command_Line.Launch), so it can be used for as long as that is.

   Empty_List : constant String_List;

   function Length (List : String_List) return Natural;

   function Element
     (List  : String_List;
      Index : Positive)
      return String;
   --  The string at Index, or "" if Index is greater than Length (List).
   --  (The run-time system has no assertions, so there is no
   --  precondition.)

private

   type String_List is
      record
         Buffer        : System.Address := System.Null_Address;
         Buffer_Length : System.Storage_Elements.Storage_Count := 0;
         First_Item    : Positive := 1;
         Count         : Natural := 0;
      end record;
   --  Items First_Item .. First_Item + Count - 1 of the buffer of
   --  Buffer_Length bytes at Buffer, in the format of the launch buffer
   --  (see Rose.Command_Line.Launch).

   Empty_List : constant String_List := (others => <>);

   function Length (List : String_List) return Natural
   is (List.Count);

end Rose.Command_Line;
