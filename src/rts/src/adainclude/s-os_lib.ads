--  Rose's System.OS_Lib: only what System.File_IO needs.
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

   function Errno return Integer;

   procedure Set_Errno (Err : Integer);

   function Errno_Message
     (Err     : Integer := Errno;
      Default : String  := "") return String;
   --  A message describing Err, or Default if there is one and Err is not
   --  known.

end System.OS_Lib;
