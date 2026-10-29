with Interfaces;
with System.Machine_Code;

package body Rose.Random is

   use Interfaces;
   use System.Machine_Code;

   Mixed_State : Unsigned_64 := 0;

   function Has_RDRAND return Boolean;
   --  CPUID leaf 1 sets ECX bit 30 if the processor has RDRAND.

   procedure Hardware_Word
     (Value   : out Unsigned_32;
      Success : out Boolean);
   --  A word from RDRAND, which can fail when its source is exhausted.

   function Mixed_Word return Unsigned_64;
   --  The timestamp counter added to the state, and mixed with
   --  SplitMix64's finaliser.

   ----------
   -- Fill --
   ----------

   procedure Fill (Item : out System.Storage_Elements.Storage_Array) is
      use System.Storage_Elements;
      Use_Hardware : constant Boolean := Has_RDRAND;
   begin
      for I in Item'Range loop
         declare
            Value   : Unsigned_32 := 0;
            Success : Boolean := False;
         begin
            if Use_Hardware then
               for Attempt in 1 .. 10 loop
                  Hardware_Word (Value, Success);
                  exit when Success;
               end loop;
            end if;

            if not Success then
               Value := Unsigned_32 (Mixed_Word and 16#FFFF_FFFF#);
            end if;

            Item (I) := Storage_Element (Value and 16#FF#);
         end;
      end loop;
   end Fill;

   -------------------
   -- Hardware_Word --
   -------------------

   procedure Hardware_Word
     (Value   : out Unsigned_32;
      Success : out Boolean)
   is
      Carry : Unsigned_8;
   begin
      Asm ("rdrand %0" & ASCII.LF & ASCII.HT & "setc %1",
           Outputs  => (Unsigned_32'Asm_Output ("=r", Value),
                        Unsigned_8'Asm_Output ("=q", Carry)),
           Volatile => True);
      Success := Carry /= 0;
   end Hardware_Word;

   ----------------
   -- Has_RDRAND --
   ----------------

   function Has_RDRAND return Boolean is
      Features : Unsigned_32;
   begin
      Asm ("movl $1, %%eax" & ASCII.LF & ASCII.HT & "cpuid",
           Outputs  => Unsigned_32'Asm_Output ("=c", Features),
           Clobber  => "eax, ebx, edx",
           Volatile => True);
      return (Features and 2 ** 30) /= 0;
   end Has_RDRAND;

   ----------------
   -- Mixed_Word --
   ----------------

   function Mixed_Word return Unsigned_64 is
      Low, High : Unsigned_32;
      Z         : Unsigned_64;
   begin
      Asm ("rdtsc",
           Outputs  => (Unsigned_32'Asm_Output ("=a", Low),
                        Unsigned_32'Asm_Output ("=d", High)),
           Volatile => True);
      Mixed_State :=
        Mixed_State + Shift_Left (Unsigned_64 (High), 32)
        + Unsigned_64 (Low) + 16#9E37_79B9_7F4A_7C15#;
      Z := Mixed_State;
      Z := (Z xor Shift_Right (Z, 30)) * 16#BF58_476D_1CE4_E5B9#;
      Z := (Z xor Shift_Right (Z, 27)) * 16#94D0_49BB_1331_11EB#;
      return Z xor Shift_Right (Z, 31);
   end Mixed_Word;

end Rose.Random;
