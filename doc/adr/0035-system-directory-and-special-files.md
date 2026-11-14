# 35. The system directory and special files

- **Status:** Proposed
- **Date:** 2026-11-14
- **Related:** #929 (this decision), [ADR 0002](0002-typed-commands-and-directory-caps.md) (directories as named caps, roles), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (rights are types), [ADR 0020](0020-clock.md) (the clock, a system service passed as a role), [ADR 0029](0029-installed-capabilities.md) (installed capabilities), [ADR 0031](0031-updating-an-installation.md) (the interfaces freeze with 0.4.0), [ADR 0036](0036-directory-views.md) (directory views, which amends sections 2 and 3)

## Context

On Unix, `cat /dev/null` reads nothing and `echo hi > /dev/null` throws the output away. `/dev/null` is a service that is already running and that anyone can name. Rose has no way to name a service like that.

- **Petal's environment is a table of named directory caps** (ADR 0002, section 5), with no root. Today the table has one entry, `home`, which petal gets from its `User` argument (`User.Home_Directory`).
- **A directory entry is a file or a directory.** `File_Kind` has a third value, `Special_File`, from ADR 0003, but nothing creates one. The directory server and isofs return it only for an index that is out of range, and `Directory` has no operation that gets one.
- **System services reach commands as roles.** The clock is launched once at install, and its `Clock` interface goes from init to login to petal. Petal then gives it to each command that declares the `Clock_Service` role (ADR 0020). This works for programs, but a user can't name a role at the prompt.
- **A command's `out` arguments go nowhere in petal.** Petal reads the mode of each argument, but it only binds `in` arguments. So launching a null stream server from the prompt would give its stream to petal, which would drop it.

### What exists today

- `interfaces/directory.idl`: `File_Kind`, `Get_Ordinary_File`, `Get_Directory`.
- `interfaces/writable_directory.idl`: `Create_File`, `Create_Directory`, `Rename`, `Delete_File` and `Delete_Directory`.
- `Petal.Session.Home_Name` is `"home"`. A path's first component names an entry in the table, and the rest of the path is looked up in that directory.
- Login launches petal with its roles' caps and its `User` argument, in the order of petal's command declaration.

### Constraints

- **No ambient authority.** Whatever is in `system` is reachable only by whoever holds the `system` cap. Petal gets that cap as it gets every other one: it is passed in.
- **Rights are types** (ADR 0012). A read-only `Directory` gives only read-only lookups for the files and directories it serves.
- **The interfaces freeze with 0.4.0** (ADR 0031). After that they can only be extended or retired, so the changes to `Directory` here belong before the 0.4.0 tag.

## Decision

### 1. A `system` directory, created at install

- **On first boot, the installer creates a directory process for `system`**, in the same way as it creates a user's home directory.
- **It fills it.** For each special file, it launches the server and adds the entry (section 3). The first special file is `null`.
- **`system` is the same for every user.** It is created once and saved like any other process, so a restore brings it back.

### 2. Petal gets `system` alongside `home`

- **`system` is a role.** Login declares it and is given it at install, and passes it on to petal, in the same way as the clock (ADR 0020). Petal declares it too.
- **Petal gets a read-only `Directory`.** No user can add to `system`, rename it or delete it.
- **Petal names it `system`** in its table of named directory caps, next to `home`. `system/null` is then a path, like `home/notes`.
- **A command never gets `system` by default.** It gets an entry, or the directory, only as an argument, when the user names it.

### 3. Special files

A special file is a directory entry whose kind is `Special_File`. It holds a cap that the directory keeps and hands out. Unlike a file, it isn't something the directory serves.

```ada
interface Directory is
   ...
   function Get_Special_File (Index : Positive) return interface Stream;
end Directory;

interface Writable_Directory : Directory is
   ...
   procedure Add_Special_File
     (Name : String;
      File : interface Stream);
end Writable_Directory;
```

- **A special file is a `Stream`.** That is what `/dev/null`, a console or a device needs, and it is what petal can bind to a reader, a writer or a redirection.
- **A directory's rights are over the directory, not over its entries.** A `Directory` cap lets its holder list the directory and look names up. A `Writable_Directory` cap also lets them add, rename and delete entries. Neither type limits what can be done with an entry. A special file's cap carries the rights it was added with: `null` goes in as a `Stream`, so a read-only `system` gives a `Stream`. A special file that shouldn't be written goes in as a `Reader`, once special files can hold more than a `Stream`.
- **The files a directory serves are a separate case.** ADR 0003, section 1, has a read-only `Directory` return read-only files and directories. That is the directory server's choice for what it serves, made because its children's write rights come from the same place as its own. It is not a rule that a directory's type bounds every entry.
- **`Get_Special_File` fails** for an entry that isn't a special file, as `Get_Ordinary_File` does for one that isn't a file.
- **Deleting a special file removes the entry**, and deletes the directory's copy of the cap. It doesn't end the server: the directory didn't launch it. `Delete_File` does this for a special file too.
- **isofs serves no special files**, and its `Get_Special_File` always fails.

### 4. `Rose.Core.Null_Stream`

- **A command with the `Server` aspect**, like `Rose.Core.File`, in `src/drivers/null_stream`. Its one argument is `Served : out Stream`.
- **`Read` gives end of file:** `Last := Buffer'First - 1`.
- **`Write` succeeds and does nothing else.**
- **It has no state**, so restoring it, or a new version taking over from it, needs nothing from the old one.
- **One process serves every reader and writer.** The installer launches it once and adds it to `system` as `null`.

### 5. Petal resolves special files

- **For an argument of type `Reader` or `Writer`**, a path that names a special file gives that part of the `Stream`. A `Stream` client includes the caps of `Reader` and `Writer`, so narrowing it is free (ADR 0003, section 1). `cat system/null` reads nothing.
- **For a redirection,** `echo hi > system/null` writes to the special file's `Writer`, rather than creating a file. A redirection to an existing special file never replaces it.
- **For an argument that takes a `File` or a `Directory`**, a special file is an error, which says what the entry is.
- **`list` shows a special file** with its kind, as it shows a directory.

### 6. Order of work

1. This ADR.
2. `Directory.Get_Special_File`, `Writable_Directory.Add_Special_File`, and their implementations in the directory server and isofs.
3. `Rose.Core.Null_Stream`, with a self test that reads and writes it through the generated client.
4. The installer creates `system` and adds `null`, and login and petal get the role.
5. Petal resolves special files (section 5), with a boot test of `cat system/null` and `echo hi > system/null`.
6. The book: special files in the chapter on files and directories, and then the parked chapter on building programs, which uses `Rose.Core.Null_Stream` as its server example.

## Consequences

- **Users get a `/dev/null`**, reached by name and through the same lookup as any file, with no global namespace.
- **Later devices have somewhere to live.** A serial port or a second console can be a special file in `system` without a new mechanism.
- **`Directory` gains an operation**, which every directory server implements. Today there are two: the directory server and isofs.
- **Login and petal gain a role.** Their callers pass one more cap, in declaration order.
- **The rights of a directory and of its entries are separate.** This ADR states that explicitly. ADR 0003's read-only lookups stay as the rule for the files and directories a directory serves.

## Alternatives considered

- **A role only, like the clock.** Commands that declare a `Null_Stream` role would get the cap from petal. This needs no change to `Directory`, but nobody can name it at the prompt, so it isn't a `/dev/null`.
- **A name that petal knows,** such as `@null`. The user can type it, but it is new syntax and a special case in petal. Each later device would need another one.
- **Petal binds a command's `out` arguments to names.** That is worth doing in its own right, but it gives each session its own null stream, launched by hand, rather than one that is always there.
- **Special files of any interface.** An entry could hold a cap of any interface, with its interface's name, and petal would check it against the argument's type. This is more general, but the IDL has no untyped interface return, and every use today is a stream.

## Open questions

- **Who holds the writable `system`?** The installer could keep it, drop it after install, or give it to an administrator role (ADR 0021) so that devices can be added later.
- **More named directories.** Should petal's table get other entries from the system as well, such as a shared `tmp`, or should those be up to the user's `.petalrc`?

## Follow-up work

- #929: the implementation, in the order of section 6.
- The parked book chapter on building programs.
