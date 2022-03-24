with Tests.Smoke;
with Tests.Stack;

package Tests.Suite is

   Test_Suite : constant array (Positive range <>) of Test_Function :=
                  (Tests.Smoke.Smoke_Test'Access,
                   Tests.Stack.Stack_Test'Access);

end Tests.Suite;
