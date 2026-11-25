with Ada.Exceptions;

private with Ada.Strings.Unbounded;
private with System.Storage_Elements;

private with Rose.Command_Signatures;

package Rose.Command_Line.Host is

   --  The launch of a command built for the host's own Ada run-time
   --  (#1101). The main that idle --target host generates calls Receive
   --  with the command's metadata, then opens the host backing of each
   --  interface argument and role member with the text that
   --  Interface_Argument gives, calls the command, and calls Complete; if
   --  the command raises Command_Failed, it calls Fail instead. The
   --  places of the arguments are those of a launch on Rose (see
   --  Rose.Command_Line.Launch).
   --
   --  Receive binds the process's command line as petal binds a command
   --  line: options by name, as --name=VALUE or --name VALUE, with
   --  --no-name for a Boolean, and -x for an argument with a short name;
   --  then the other arguments by position, in declaration order, with a
   --  String_List taking the rest; "--" ends the options. Case doesn't
   --  matter in a name, and a hyphen is the same as an underscore. An
   --  argument that isn't given takes its default. An in Directory or
   --  Writable_Directory that isn't given is the current directory, and a
   --  reader whose default is Standard_Input is standard input: for both,
   --  Interface_Argument gives "". --help and --version write what petal
   --  writes for the command.

   Stopped : exception;
   --  Raised by Receive when the command isn't to run: after --help or
   --  --version, with a success status, or when the command line is
   --  wrong, once it has said why on standard error, with status 2.

   type Launch_Call is limited private;

   procedure Receive
     (Call     : in out Launch_Call;
      Metadata : String);
   --  Bind the process's command line to the arguments of the command
   --  whose metadata is Metadata.

   function Boolean_Argument
     (Call : Launch_Call;
      Word : Natural)
      return Boolean;

   function Natural_Argument
     (Call : Launch_Call;
      Word : Natural)
      return Natural;

   function String_Argument
     (Call : Launch_Call;
      Item : Positive)
      return String;

   function String_List_Argument
     (Call       : Launch_Call;
      Item       : Positive;
      Count_Word : Natural)
      return String_List;
   --  The strings from Item, as many as data word Count_Word says. The
   --  list refers to Call, so it can be used for as long as Call is.

   function Interface_Argument
     (Call : Launch_Call;
      Cap  : Natural)
      return String;
   --  The text given for the interface argument in the cap place Cap: a
   --  path, or "" if it wasn't given, or if Cap is a role member's.

   procedure Complete (Call : in out Launch_Call);
   --  The command completed. Its exit status is success.

   procedure Fail
     (Call    : in out Launch_Call;
      Failure : Ada.Exceptions.Exception_Occurrence);
   --  The command raised Failure, Command_Failed: write its message on
   --  standard error, after the command's name, as petal does, and make
   --  the exit status a failure.

private

   Max_Words : constant := 32;
   Max_Caps  : constant := 16;
   Max_Items : constant := 64;

   type Word_Array is array (0 .. Max_Words - 1) of Natural;

   type Text_Array is
     array (Natural range <>) of Ada.Strings.Unbounded.Unbounded_String;

   type Buffer_Access is access System.Storage_Elements.Storage_Array;

   type Launch_Call is limited
      record
         Signature : Rose.Command_Signatures.Signature;
         Words     : Word_Array := (others => 0);
         Caps      : Text_Array (0 .. Max_Caps - 1);
         Buffer    : Buffer_Access;
         --  The string arguments, as the items of a launch buffer, so
         --  that a String_List is the same as on Rose.
      end record;

end Rose.Command_Line.Host;
