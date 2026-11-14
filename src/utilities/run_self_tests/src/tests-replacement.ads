package Tests.Replacement is

   function Replacement_Test return Boolean;

   function Replace_Directory_Test return Boolean;
   --  Launch a directory against a test bank, with a file and an empty
   --  subdirectory in it, and keep writable and read-only caps to them.
   --  Replace the directory with a new version, which is given Previous
   --  alone (ADR 0032, #831), and check that the new version has the
   --  entries, that its caps to them work, and the caps from before, and
   --  that entries can still be created, renamed and deleted: deleting
   --  one that the old version created ends it. Skipped without a
   --  Replacer.

   function Replace_File_Test return Boolean;
   --  Replace a running file server through mem's Replacer (ADR 0032,
   --  #867), with a writer, a reader and a version open: after the
   --  handover, the writer still writes, the reader goes on from where it
   --  was, the version still commits, and the file has every byte.
   --  Skipped without the Replace_Process role.

   function Replace_Process_Test return Boolean;
   --  Replace a running replaceable_test server through mem's Replacer
   --  (ADR 0032, #866): its clients' caps reach the new version, which
   --  answers the call it deferred and a call made during the handover,
   --  and a refused replacement leaves the old version running, still
   --  owing its deferred call. Skipped without the Replace_Process role.
   --  The Replaceable interface (ADR 0032, #865), with the test server
   --  replaceable_test:
   --
   --  - launch one, and read its state twice, which is the same both
   --    times, and does not change what it serves;
   --  - get its caps, which are one cap set server Constructor, and check
   --    that it works;
   --  - launch a second one with the first's Replaceable as its Previous
   --    argument, and check that it serves the first one's name and count,
   --    and that the first one still serves the same.
   --
   --  Skipped without the Cap_Set_Service role, as when petal runs it.

   function Replace_User_Test return Boolean;
   --  Launch a user server, with a home directory, against a test bank,
   --  and start a session. Replace the server with a new version, which
   --  is given Previous alone (ADR 0032, #831), and check that the old
   --  version's User cap reaches the new version, which has the user's
   --  name, home directory and bank: a file can be created in the home
   --  directory, and both the session from before and a new one work.
   --  Skipped without a Replacer.

end Tests.Replacement;
