with Tests.Directories;
with Tests.Dispatching;
with Tests.Files;
with Tests.Smoke;
with Tests.Stack;
with Tests.Users;

package Tests.Suite is

   Test_Suite : constant array (Positive range <>) of Test_Function :=
                  (Tests.Smoke.Smoke_Test'Access,
                   Tests.Stack.Stack_Test'Access,
                   Tests.Dispatching.Dispatching_Test'Access,
                   Tests.Files.File_Test'Access,
                   Tests.Directories.Directory_Test'Access,
                   Tests.Users.User_Test'Access);

end Tests.Suite;
