package Rose.Kernel.Command_Line
  with SPARK_Mode,
       Abstract_State    => Command_Line_State,
       Initializes       => Command_Line_State,
       Initial_Condition => Valid
is

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   function Valid return Boolean
   with Ghost, Global => (Input => Command_Line_State);
   --  The arguments that have been added are within the text that holds
   --  them.  Only this package changes the state, and every operation
   --  keeps it valid, so this holds whenever a caller can see it: it is a
   --  precondition so that the proof can use it.

   function Argument_Count return Natural
   with Global => (Input => Command_Line_State),
        Pre    => Valid;

   procedure Get_Argument (Index     : in  Positive;
                           Text      : out String;
                           Last      : out Natural)
   with Global => (Input => Command_Line_State),
        Pre    => Valid and then Text'First >= 1,
        Post   => Last >= Text'First - 1
                  and then Last - Text'First + 1 <= Text'Length
                  and then (if Index > Argument_Count
                            then Last = Text'First - 1);
   --  The name of argument Index, cut to fit Text, which is blank after
   --  it.  Last is Text'First - 1 for an argument that is not there.

   function Have_Argument
     (Name : String)
      return Boolean
   with Global => (Input => Command_Line_State),
        Pre    => Valid,
        Post   => (if Have_Argument'Result then Argument_Count > 0);

   function Integer_Argument
     (Name : String)
      return Integer
   with Global => (Input => Command_Line_State),
        Pre    => Valid,
        Post   => Integer_Argument'Result >= 0;
   --  The value of Name=Value, or 0 if there is no such argument, it has
   --  no value, the value has anything but digits in it, or it is too
   --  large for an Integer.

   procedure Initialise_Command_Line
   with Global => (In_Out => Command_Line_State),
        Pre    => Valid,
        Post   => Valid;
   --  Adds the kernel's arguments from the boot loader.  Its body, which
   --  prints each argument and passes the adder to Rose.Multiboot, is not
   --  in SPARK, so this contract is assumed.

end Rose.Kernel.Command_Line;
