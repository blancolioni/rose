with Ada.Exceptions;

with Rose.Addresses;

package body Rose.CRTL is

   Current_Top : Rose.Addresses.Virtual_Page_Address := 0
     with Unreferenced;

   function Allocate
     (Size : System.Storage_Elements.Storage_Count)
     return System.Address
   is
      pragma Unreferenced (Size);
   begin
      return System.Null_Address;
   end Allocate;

   procedure Deallocate
     (Ptr : System.Address)
   is null;

   procedure Put_Char_Stderr
     (Ch : Character);

   pragma Export (C, Put_Char_Stderr, "put_char_stderr");

   procedure Unwind_Resume is null;

   pragma Export (C, Unwind_Resume, "_Unwind_Resume");

   procedure Put_Char_Stderr
     (Ch : Character)
   is null;

   procedure Gnat_Personality_V0 is null;

   pragma Export (C, Gnat_Personality_V0, "__gnat_personality_v0");

   procedure Unhandled_Terminate;
--  pragma No_Return (Unhandled_Terminate);
   pragma Export (C, Unhandled_Terminate, "__gnat_unhandled_terminate");

   function Exception_Message_Length
     (X : Ada.Exceptions.Exception_Occurrence)
     return Natural;
   pragma Export (Ada, Exception_Message_Length, "__gnat_exception_msg_len");

   --  procedure Append_Info_Exception_Message
   --    (X    : Ada.Exceptions.Exception_Occurrence;
   --     Info : in out String;
   --     Ptr  : in out Natural);
   --  pragma Export
   --    (Ada, Append_Info_Exception_Message, "__gnat_append_info_e_msg");

   procedure Append_Info_Untailored_Exception_Information
     (X    : Ada.Exceptions.Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural);
   pragma Export
     (Ada, Append_Info_Untailored_Exception_Information,
      "__gnat_append_info_u_e_info");

   Gnat_Argv : System.Address;
   pragma Export (C, Gnat_Argv, "gnat_argv");

   procedure Fill_Arg (A : System.Address; Arg_Num : Integer);
   pragma Export (C, Fill_Arg, "__gnat_fill_arg");

   function Len_Arg (Arg_Num : Integer) return Integer;
   pragma Export (C, Len_Arg, "__gnat_len_arg");

   procedure Unhandled_Terminate is null;

   function Exception_Message_Length
     (X : Ada.Exceptions.Exception_Occurrence)
     return Natural
   is
      pragma Unreferenced (X);
   begin
      return 0;
   end Exception_Message_Length;

   --  procedure Append_Info_Exception_Message
   --    (X    : Ada.Exceptions.Exception_Occurrence;
   --     Info : in out String;
   --     Ptr  : in out Natural)
   --  is null;

   procedure Append_Info_Untailored_Exception_Information
     (X    : Ada.Exceptions.Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural)
   is null;

   procedure Fill_Arg (A : System.Address; Arg_Num : Integer)
   is null;

   function Len_Arg (Arg_Num : Integer) return Integer is
      pragma Unreferenced (Arg_Num);
   begin
      return 0;
   end Len_Arg;

end Rose.CRTL;
