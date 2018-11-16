with System;

with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;

package Init.Commands is

   type Init_Command is (Halt, Nop, Invoke, Move, Copy);

   type Init_Register is range 0 .. 4000;

   type Value_Class is (Cap_Value, Word_Value, Address_Value,
                        Object_Id_Value, String_Value);

   type Value_Record (Class : Value_Class := Word_Value) is
      record
         case Class is
            when Cap_Value =>
               Cap    : Rose.Capabilities.Capability :=
                          Rose.Capabilities.Null_Capability;
            when Word_Value =>
               W      : Rose.Words.Word := 0;
            when Address_Value =>
               Addr   : System.Address;
            when Object_Id_Value =>
               Object : Rose.Objects.Object_Id;
            when String_Value =>
               Text   : access constant String;
         end case;
      end record;

   type Registers is array (Init_Register) of Value_Record;

   Max_Command_Arguments : constant := 15;

   type Command_Arguments is
     array (1 .. Max_Command_Arguments) of Init_Register;

   type Command_Record (Command : Init_Command := Halt) is
      record
         case Command is
            when Halt =>
               null;
            when Nop =>
               null;
            when Invoke =>
               Cap            : Init_Register := 0;
               Endpoint       : Rose.Objects.Endpoint_Id := 0;
               Block          : Boolean;
               Send_Arg_Count : Natural := 0;
               Recv_Arg_Count : Natural := 0;
               Send           : Command_Arguments := (others => 0);
               Recv           : Command_Arguments := (others => 0);
            when Move =>
               Target_R       : Init_Register;
               Source_R       : Init_Register;
               Source_W       : Value_Record;
            when Copy =>
               To_Buffer      : Init_Register;
               From_Buffer    : Init_Register;
               Length         : Init_Register;
         end case;
      end record;

end Init.Commands;
