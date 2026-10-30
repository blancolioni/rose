with Tests.Clock;
with Tests.Containers;
with Tests.Directories;
with Tests.Dispatching;
with Tests.Exceptions;
with Tests.Files;
with Tests.Finalization;
with Tests.Heap;
with Tests.Invalid_Caps;
with Tests.Memory;
with Tests.Secondary_Stack;
with Tests.Server;
with Tests.Smoke;
with Tests.Stack;
with Tests.Strings;
with Tests.Streams;
with Tests.Users;

package Tests.Suite is

   Test_Suite : constant array (Positive range <>) of Test_Function :=
                  (Tests.Smoke.Smoke_Test'Access,
                   Tests.Stack.Stack_Test'Access,
                   Tests.Secondary_Stack.Secondary_Stack_Test'Access,
                   Tests.Strings.String_Test'Access,
                   Tests.Server.Instance_Test'Access,
                   Tests.Heap.Heap_Test'Access,
                   Tests.Dispatching.Dispatching_Test'Access,
                   Tests.Exceptions.Exception_Test'Access,
                   Tests.Streams.Stream_Test'Access,
                   Tests.Containers.Container_Test'Access,
                   Tests.Finalization.Finalization_Test'Access,
                   Tests.Files.File_Test'Access,
                   Tests.Directories.Directory_Test'Access,
                   Tests.Users.User_Test'Access,
                   Tests.Clock.Calendar_Test'Access,
                   Tests.Clock.Clock_Test'Access,
                   Tests.Invalid_Caps.Invalid_Cap_Test'Access,
                   Tests.Invalid_Caps.No_Buffer_Test'Access,
                   Tests.Memory.Copy_Test'Access);

end Tests.Suite;
