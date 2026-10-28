# 12. Argument modes, writable interfaces and transactional commands

- **Status:** Proposed
- **Date:** 2026-10-28
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands, sections 1, 4 and 5), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), #141 (shell-owned commit), #345 (petal binds writable directories), Rose.Core project (#332 to #337)

## Context

ADR 0002 has an interface argument's mode say three things at once.

- **The right.** The table in section 1 has `in Directory` give a lookup-only cap and `in out Directory` give a cap that can also create and modify entries.
- **The commit.** Section 4 has the shell name an `out File` and commit an `in out File` only if the command completes.
- **The direction.** Idle, like Ada, uses `out` for a value the command returns. An `out` interface argument that isn't a file or directory is a cap result.

These have come apart:

- **The type now gives the right.** Section 5 asks for `Directory` to be split so that "the type itself gives the right", and ADR 0003 did this: `Directory` is lookup only, and `Writable_Directory` adds `Create_File` and `Create_Directory`. Since then, `in out Directory` means nothing on its own, since no operation of `Directory` changes the directory, whatever the mode. Idle generates a `Directory` client for it, so a command declared as ADR 0002 spells it can't create a file.
- **`out` points both ways.** For a file, the shell creates the object and hands it to the command. For any other interface, the command hands a cap back. A command can't return a writable cap, because `out Writable_File` would mean the commit.
- **The commit isn't a property of one argument.** A command that writes two files should commit both or neither. A command that reads one file while it writes another should see a read that is consistent with the write. Neither can be said one argument at a time.
- **Live changes wait for the commit.** With the right and the commit in the same mode, a command can't be given a directory it changes as it runs. Every writable grant waits for #141, including `touch` and `mkdir`, which have nothing to commit.

A shell needs to know several things about an argument: which way its cap flows, what the command may do with it, when the command's changes take effect, which name it binds to and whether that name must exist, and how long it stays granted. Three modes can't carry all of these, so each goes where it belongs.

## Decision

### 1. The mode is the direction

A command argument is either `in` or `out`, with the meaning those modes have in Ada.

| Mode | For a value | For an interface |
|---|---|---|
| `in` | The shell supplies it. | The shell supplies a cap. |
| `out` | The command returns it, as a typed result. | The command returns a cap, which is a cap result, as idle has it now. This holds for every interface, writable or not, so `File : out Writable_File` returns a writable file. |

**`in out` is not allowed on a command argument.** Nothing a command does needs it.
- A value the command changes is an `in` and an `out`.
- A cap the command changes is an `in` of a `Versioned` type.
- A cap the command replaces is an `out`.

Idle accepts `in out` on command arguments today, and no Rose command uses it.

### 2. The type is the right

An argument's interface type says what the command can do with the object. A command that creates entries in a directory takes a `Writable_Directory`. A command that writes a file takes a `Writable_File`.

An interface argument is a cap, which is a reference, and the mode of a reference doesn't limit what can be done through it. Ada works the same way: `Ada.Text_IO.Put` takes its `File` with mode `in`, and still writes to the file.

An interface whose objects can be versioned and committed is marked with the `Versioned` aspect:

```ada
interface Writable_File : File, Stream_Writer
  with Versioned
is
end Writable_File;
```

- **Inherited.** An interface that inherits from a `Versioned` interface is `Versioned` too, without repeating the aspect.
- **Explicit.** Idle can't tell from an interface's operations whether they change its object: `Create_File` and `Create_Directory` are functions, and they change the directory. So the interface states it.
- **Named for what the shell does.** The aspect says that the shell can version and commit the object, not that a command can write to it. `Stream_Writer` can be written to, and is deliberately not `Versioned`.
- **An aspect, not an interface (#421).**
  - A `Versioned` interface for `Writable_File` and `Writable_Directory` to inherit would say how the shell treats an object, not what a command can do with it, so it would break section 2's rule that the type is the right.
  - `X : in Versioned` would be a legal argument that means nothing.
  - If the interface carried the versioning protocol, every command given a version could commit or discard it itself, which defeats section 3.
  - If #141 needs a uniform protocol for versions, it goes on a separate interface that only the shell and the persistence layer hold.
- **It replaces the names `File` and `Directory`** in idle's `Grants_Right`, which treats those two interfaces and their descendants specially. With section 1, interface arguments need no special case for direction. `Versioned` only decides which arguments a transaction covers (section 3).

### 3. The command is the transaction

By default a command runs as a transaction over its `Versioned` arguments:

- **Before it starts,** the shell gives it a version of each `Versioned` object it names, not the object itself.
- **If the command completes,** the shell commits every version together.
- **If it fails,** the shell discards them all, and the objects are as they were before the command ran.

A failed command leaves nothing behind, which is the benefit ADR 0002 promised from section 4.

A command that changes its objects as it runs declares `Live`:

```ada
command Rose.Core.Touch
  with Summary => "Create an empty file.",
       Live
is
   Name : String
     with Help => "the name of the new file";
   Directory : in Writable_Directory
     with Help => "the directory to create it in";
end Rose.Core.Touch;
```

- **A live command** gets the shell's own caps, and its changes take effect as it makes them. If it fails part way, it can leave its objects changed.
- **Until #141,** petal can't make versions. It refuses to run a command that is transactional by default and has `Versioned` arguments, rather than running it live without saying so. The Rose.Core commands that change a directory (`touch`, `mkdir`, `rename`, `remove` and `rmdir`) declare `Live`.
- **Streams are never versioned.** A stream's data is gone once it's read, and anything written to it has already been seen. So the standard streams stay live in a transactional command. Standard error is a log that survives the command's failure.

`copy` needs no special mode:

```ada
command Rose.Core.Copy
  with Summary => "Copy a file."
is
   Source : in File
     with Help => "the file to copy";
   Target : in Writable_File
     with Help => "the new file";
end Rose.Core.Copy;
```

### 4. The shell binds names

Which object an argument names, and whether it may exist already, depends on how the user writes it, not on the signature.

- **A word for a `Writable_Directory`** names a directory in petal's table, as for `in Directory`, and defaults to the current directory, or home until there is one. Petal already holds a `Writable_Directory` client for each of its directories.
- **A word for a `Writable_File`** names a file in the current directory. For a transactional command, a name that doesn't exist gets a new, empty version, which is named when it commits, so copying to a new file and copying over an existing one are the same command. For a live command, the file must exist.
- **Whether an existing file may be replaced,** as with a noclobber option, or whether output is appended (`>>`), is the shell's choice at the prompt (#338).

### 5. Changes to ADR 0002

This ADR replaces these parts of ADR 0002. ADR 0002 points here:

- **section 1:** the table of modes, and the rows for `out File`, `in out File`, `in Directory` and `in out Directory`;
- **section 4:** the commit is the command's, over all its `Versioned` arguments, not one argument's `out` or `in out` mode;
- **section 5:** a directory grant is `in Directory` or `in Writable_Directory`, not `in out Directory`.

## Consequences

**Benefits**
- Each property has one place: the mode for the direction, the type for the right, the command for the transaction, and the shell for names.
- A transactional command commits all of its changes or none of them, including changes across several files and directories.
- `touch`, `mkdir`, `rename`, `remove` and `rmdir` can be written now, as live commands, without waiting for #141.
- A command can return a writable cap.
- The generated parameter's type is the argument's type. Idle doesn't map one interface to another.

**Costs**
- **Idle:**
  - the `Versioned` interface aspect, with its inheritance;
  - the `Live` command aspect;
  - `in out` command arguments become an error, for values as well as interfaces;
  - `out` interface arguments become cap results for every interface;
  - idle's command tests that use `in out` or `out File` change.
- **Interfaces:** `writable_file.idl` and `writable_directory.idl` gain the `Versioned` aspect.
- **Most commands that write are blocked until #141,** unless they declare `Live`. That is deliberate: a command shouldn't run live without saying so.
- **ADR 0002's examples are out of date** where they use `out File`, `in out File` or `in out Directory`.

## Open questions

- **A live log in a transactional command.** A build commits its outputs together, but its log should survive a failure. This is like the Writer monad: an effect that sits outside the transaction and is kept whatever the result. Standard error already works this way (section 3). A command could perhaps declare more log streams as roles. Making one `Versioned` argument live inside a transactional command can wait for a real case.
- **Snapshot reads.** A transactional command's `in File` is the live file. A read consistent with the command's writes needs a version of a `File`, which isn't `Versioned`. This waits for the persistence layer to version any object.
- **Directory versions.** What does a version of a `Writable_Directory` hold, and what does committing it do when the command has also written files inside it? This is part of #141.
- **Narrower grants.** A `Writable_Directory` can create entries, and with #346 delete and rename them too. Should `touch` get a grant that can only create? This is ADR 0002's open question on attenuation. #422 proposes arguments typed by a single method, such as `in Writable_Directory.Create_File`.

## Follow-up work

- Idle: the `Versioned` and `Live` aspects and the argument rules in section 1 (blancolioni/idle, issue to be opened).
- Mark `Writable_File` and `Writable_Directory` as `Versioned`.
- Petal: bind `in Writable_Directory` and `in Writable_File`, and refuse transactional commands with `Versioned` arguments until #141 (#345).
- Update the Rose.Core issues (#332 to #337) to the new spellings.
- #141: versions and the commit, for transactional commands.
