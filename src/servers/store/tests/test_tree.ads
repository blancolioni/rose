with Store.Page_Trees;

with Test_Device;
with Test_Space;

--  A small cache, so that the tests evict pages all the time.

package Test_Tree is
  new Store.Page_Trees
    (Cache_Size => 12,
     Read       => Test_Device.Read,
     Write      => Test_Device.Write,
     Allocate   => Test_Space.Allocate,
     Free       => Test_Space.Free,
     Is_Born    => Test_Space.Is_Born);
