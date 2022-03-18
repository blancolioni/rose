with Tests.Smoke;

package Tests.Suite is

   Test_Suite : constant array (Positive range <>) of Test_Function :=
                  (1 => Tests.Smoke.Smoke_Test'Access);

end Tests.Suite;
