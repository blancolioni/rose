--  Rose's System.OS_Lib: what System.File_IO and Ada.Calendar need.
--
--  GCC's System.OS_Lib is a binding to a POSIX-like operating system, and
--  Rose is not one. Errno is the error of the last failed operation in
--  Interfaces.C_Streams, which sets it with Set_Errno.

package System.OS_Lib is
   pragma Preelaborate;

   ENOENT : constant := 2;
   EIO    : constant := 5;
   EBADF  : constant := 9;
   EINVAL : constant := 22;
   ENOSYS : constant := 38;

   type OS_Time is private;
   --  A time in seconds since 1970, as Ada.Calendar passes it to the C
   --  routine that finds the time zone (#497).

   function To_Ada (Time : Long_Long_Integer) return OS_Time;

   function Errno return Integer;

   procedure Set_Errno (Err : Integer);

   function Errno_Message
     (Err     : Integer := Errno;
      Default : String  := "") return String;
   --  A message describing Err, or Default if there is one and Err is not
   --  known.

private

   type OS_Time is new Long_Long_Integer;

end System.OS_Lib;
