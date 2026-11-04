with System.Storage_Elements;

with Rose.Init_Capabilities;
with Rose.Invocation;
with Rose.System_Calls;

with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Storage.Client;

with Init.Calls;

package body Init.Grants is

   use Rose.Words;
   use type System.Storage_Elements.Storage_Element;

   Framebuffer_Tag : constant := 8;
   --  The multiboot2 framebuffer information tag

   Tag_Buffer : System.Storage_Elements.Storage_Array (0 .. 31);
   --  Room for the start of the framebuffer tag, up to its type.  It is
   --  not on the stack, which is small in init.

   Max_Grants : constant := 32;

   type Grant_Kind is (Port_Grant, Interrupt_Grant, Page_Grant);

   type Grant_Record is
      record
         Kind        : Grant_Kind;
         First, Last : Word;
         Owner       : Owner_Id;
      end record;

   Grants      : array (1 .. Max_Grants) of Grant_Record;
   Grant_Count : Natural := 0;

   Last_Owner : Owner_Id := 1000;

   function Claim
     (Kind        : Grant_Kind;
      Owner       : Owner_Id;
      First, Last : Word)
      return Boolean;
   --  Record that Owner has First .. Last of Kind, and return True, unless
   --  another owner has any of them, or there is no room.

   function Read_Framebuffer_Tag return Boolean;
   --  Read the start of the framebuffer tag into Tag_Buffer, and return
   --  True if it is an RGB framebuffer below 4 GB.  The tag has the
   --  address as 64 bits at 8, the pitch at 16, the width at 20, the
   --  height at 24, the bits per pixel at 28, and the type at 29, which
   --  is 1 for RGB.

   function Get_Word
     (Offset : System.Storage_Elements.Storage_Offset)
      return Word;
   --  The little-endian word at Offset in Tag_Buffer

   ----------
   -- Bank --
   ----------

   function Bank
     (Storage : Rose.Capabilities.Capability;
      Pages   : Rose.Words.Word;
      Regions : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
      use Rose.Interfaces.Bank.Client;
      use Rose.Interfaces.Storage.Client;
      use type Rose.Capabilities.Capability;
      Store : Storage_Client;
   begin
      if Storage = Rose.Capabilities.Null_Capability
        or else Pages = 0
        or else Regions = 0
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      Open (Store, Storage);
      return Get_Interface_Cap
        (Create_Sub_Bank
           (Get_Root_Bank (Store), Word_64 (Pages), Regions));
   end Bank;

   --------------
   -- Boot_Tag --
   --------------

   function Boot_Tag
     (Tag : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
   begin
      return Init.Calls.Call (Rose.Init_Capabilities.Boot_Tags_Cap, Tag);
   end Boot_Tag;

   -----------
   -- Claim --
   -----------

   function Claim
     (Kind        : Grant_Kind;
      Owner       : Owner_Id;
      First, Last : Word)
      return Boolean
   is
   begin
      if Owner = No_Owner then
         return True;
      end if;

      for Grant of Grants (1 .. Grant_Count) loop
         if Grant.Kind = Kind
           and then Grant.Owner /= Owner
           and then First <= Grant.Last
           and then Last >= Grant.First
         then
            return False;
         end if;
      end loop;

      if Grant_Count = Max_Grants then
         return False;
      end if;

      Grant_Count := Grant_Count + 1;
      Grants (Grant_Count) := (Kind, First, Last, Owner);
      return True;
   end Claim;

   -----------------
   -- Device_Page --
   -----------------

   function Device_Page
     (Owner : Owner_Id;
      Page  : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
   begin
      if not Claim (Page_Grant, Owner, Page, Page) then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Init.Calls.Call
        (Rose.Init_Capabilities.Device_Pages_Cap, (1 => Page));
   end Device_Page;

   -----------------
   -- Framebuffer --
   -----------------

   function Framebuffer
     (Owner : Owner_Id)
      return Rose.Capabilities.Capability
   is
      Page_Bytes : constant := 4096;
   begin
      if not Read_Framebuffer_Tag then
         return Rose.Capabilities.Null_Capability;
      end if;

      declare
         Address : constant Word_64 := Word_64 (Get_Word (8));
         Length  : constant Word_64 :=
                     Address mod Page_Bytes
                     + Word_64 (Get_Word (16)) * Word_64 (Get_Word (24));
         First   : constant Word := Word (Address / Page_Bytes);
         Count   : constant Word_64 :=
                     (Length + Page_Bytes - 1) / Page_Bytes;
      begin
         if Length = Address mod Page_Bytes
           or else Word_64 (First) + Count > 2 ** 32 / Page_Bytes
           or else not Claim (Page_Grant, Owner,
                              First, First + Word (Count) - 1)
         then
            return Rose.Capabilities.Null_Capability;
         end if;

         return Init.Calls.Call
           (Rose.Init_Capabilities.Device_Ranges_Cap,
            (First, Word (Count)));
      end;
   end Framebuffer;

   --------------
   -- Get_Word --
   --------------

   function Get_Word
     (Offset : System.Storage_Elements.Storage_Offset)
      return Word
   is
      use System.Storage_Elements;
      Result : Word := 0;
   begin
      for I in reverse Offset .. Offset + 3 loop
         Result := Result * 256 + Word (Tag_Buffer (I));
      end loop;
      return Result;
   end Get_Word;

   ---------------------
   -- Has_Framebuffer --
   ---------------------

   function Has_Framebuffer return Boolean is
   begin
      return Read_Framebuffer_Tag and then Tag_Buffer (28) = 32;
   end Has_Framebuffer;

   ---------------
   -- Interrupt --
   ---------------

   function Interrupt
     (Owner  : Owner_Id;
      Vector : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
   begin
      if not Claim (Interrupt_Grant, Owner, Vector, Vector) then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Init.Calls.Call
        (Rose.Init_Capabilities.Interrupts_Cap, (1 => Vector));
   end Interrupt;

   ----------------
   -- Next_Owner --
   ----------------

   function Next_Owner return Owner_Id is
   begin
      Last_Owner := Last_Owner + 1;
      return Last_Owner;
   end Next_Owner;

   -----------
   -- Ports --
   -----------

   function Ports
     (Owner    : Owner_Id;
      Endpoint : Rose.Words.Word;
      Size     : Rose.Words.Word;
      First    : Rose.Words.Word;
      Last     : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
   begin
      if First > Last
        or else not Claim (Port_Grant, Owner, First, Last)
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Init.Calls.Call
        (Rose.Init_Capabilities.Ports_Cap, (Endpoint, Size, First, Last));
   end Ports;

   --------------------------
   -- Read_Framebuffer_Tag --
   --------------------------

   function Read_Framebuffer_Tag return Boolean is
      use type Rose.Capabilities.Capability;
      Reader : constant Rose.Capabilities.Capability :=
                 Boot_Tag (Framebuffer_Tag);
      Params : aliased Rose.Invocation.Invocation_Record;
      Size   : Word := 0;
   begin
      if Reader = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Reader);
      Rose.System_Calls.Send_Buffer
        (Params, Tag_Buffer'Length, Tag_Buffer'Address, Writable => True);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
      then
         Size := Params.Data (0);
      end if;
      Rose.System_Calls.Delete_Cap (Reader);

      return Size >= 30
        and then Tag_Buffer (29) = 1
        and then Get_Word (12) = 0;
   end Read_Framebuffer_Tag;

end Init.Grants;
