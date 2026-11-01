package body System.OS_Lib is

   Last_Errno : Integer := 0;

   -----------
   -- Errno --
   -----------

   function Errno return Integer is
   begin
      return Last_Errno;
   end Errno;

   -------------------
   -- Errno_Message --
   -------------------

   function Errno_Message
     (Err     : Integer := Errno;
      Default : String  := "") return String
   is
   begin
      case Err is
         when ENOENT =>
            return "No such file or directory";
         when EIO =>
            return "Input/output error";
         when EBADF =>
            return "Bad file descriptor";
         when EINVAL =>
            return "Invalid argument";
         when ENOSYS =>
            return "Function not implemented";
         when others =>
            return Default;
      end case;
   end Errno_Message;

   ---------------
   -- Set_Errno --
   ---------------

   procedure Set_Errno (Err : Integer) is
   begin
      Last_Errno := Err;
   end Set_Errno;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Time : Long_Long_Integer) return OS_Time is
   begin
      return OS_Time (Time);
   end To_Ada;

end System.OS_Lib;
