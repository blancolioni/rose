# 3. Directories, files and users

- **Status:** Proposed
- **Date:** 2026-10-25
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands and directory caps), #222 (Directories, files and users), #139 (split the Directory interface), #141 (shell-owned commit), Petal Shell project

## Context

ADR 0002 says that directories are independent capabilities, that the shell keeps a table of named directory caps, and that the `Directory` interface should be split so that the type itself gives the right: lookup only, or create and modify. None of this exists yet. This ADR sets out the first writable directories and files in Rose, the first notion of a user, and what petal does with them.

### What exists today

- **One directory server.** isofs serves the boot CD's directories through `Directory`, read-only in practice. Nothing implements `Create_File` or `Create_Directory`.
- **One combined interface.** `Directory` has both lookup (`Find_Entry`, `Get_Ordinary_File`, `Get_Directory`, `Read_File`) and creation (`Create_File`, `Create_Directory`). `File` is `Stream` plus `Seekable`, so every file cap can be written.
- **No users.** Init launches petal with a positional cap set: the three standard streams, a `current_directory` slot that is always null, system caps, the command registry's `Find` and the trace cap.
- **Petal starts with a prompt.** It prints its version and enters `Petal.Read.Reader_Loop`. There is no start-up file.
- **Interface arguments.** The IDL compiler (idle) already generates commands with interface-typed arguments, such as `Directory : in Directory`, received as caps.
- **The kernel has room for 64 processes** (`Max_Processes`, defined in both `Rose.Kernel` and `Rose.Kernel.Limits`).

### Constraints

- **Persistence.** A process's memory survives checkpoint and restore, so data held in a server's memory is persistent without an on-disk format.
- **Init runs only on a fresh boot.** After a restore, the saved processes come back and init does not run. So anything init creates exists exactly once.
- **Shared read-only pages.** mem caches and shares the pages of read-only segments (#200), so many processes running the same executable share its code.

## Decision

### 1. Read-only and read-write interfaces, with read-write inheriting read-only

The read-only interfaces keep the existing names, so `in Directory` and `in File` in a command signature (ADR 0002, section 1) mean read-only. The read-write interfaces inherit them and add what writing needs.

```ada
interface Directory is

   type File_Kind is (Directory, Ordinary_File, Special_File);

   function Directory_Entry_Count return Natural;
   function Directory_Entry_Name (Index : Positive) return String;
   function Directory_Entry_Kind (Index : Positive) return File_Kind;
   function Directory_Entry_Size (Index : Positive) return Storage_Count;

   function Find_Entry (Name : String) return Natural;

   function Get_Ordinary_File (Index : Positive) return interface File;
   function Get_Directory (Index : Positive) return interface Directory;

   function Read_File (Index : Positive) return interface Stream_Reader;

end Directory;

interface Writable_Directory : Directory is

   function Get_Writable_File
     (Index : Positive) return interface Writable_File;
   function Get_Writable_Directory
     (Index : Positive) return interface Writable_Directory;

   function Create_File (Name : String) return interface Writable_File;
   function Create_Directory
     (Name : String) return interface Writable_Directory;

end Writable_Directory;

interface File : Stream_Reader, Seekable is

   function Size return Storage_Count;

end File;

interface Writable_File : File, Stream_Writer is
end Writable_File;
```

- **Lookups on a read-only directory return read-only children.** `Get_Directory` and `Get_Ordinary_File` keep returning `Directory` and `File` on a `Writable_Directory` too, since an IDL operation can't change its return type in a child. Writable children come from the separate `Get_Writable_*` operations, which exist only on `Writable_Directory`.
- **Narrowing is free.** An interface cap is a set of endpoint caps, one for each operation. A `Writable_Directory` client includes the caps of every `Directory` operation, so a read-only client is those caps alone. This is how the shell passes `in Directory` from a writable directory it holds. The generated client needs a conversion to its parent interface for this.
- **`File` gains `Size`**, so a directory can report a child file's size without reading it.
- **Delete, rename and truncate are left out** of this milestone and added later.
- **isofs moves to the new interfaces.** It serves `Directory` and `File`, the read-only ones. This is the break that ADR 0002 lists under its costs.

### 2. One process per directory and per file

Each writable directory and each writable file is its own process: a `directory` server and a `file` server, each an ordinary executable that init installs at boot.

- **A file process holds the file's contents in its own memory**, which persists through checkpoints like any process's memory. A directory process holds its entries: each name, its kind, and the interface caps of the child process.
- **Creating an entry launches a process.** A directory process holds the launch caps for the `directory` and `file` executables. `Create_File` launches a new file process, keeps its caps in the new entry, and returns a `Writable_File` client. `Create_Directory` does the same with a new directory process.
- **Why one process per object.** The cap for a file or directory then names a process directly, with no object id in the cap to check, and the kernel's per-process accounting and revocation apply to each file. Overhead is expected to be small: the code is shared (#200), so each process costs its own data pages, its stack and a kernel process entry, not much more than the file would take on disk.
- **Each server is written as an independent process.** A file server knows nothing of its directory, and a directory server talks to its children only through their interfaces. Later, user-mode scheduling and private process tables will let a directory and its children run inside a single kernel process. Servers written this way can move there unchanged.
- **The kernel process limit is accepted for now.** The kernel has room for 64 processes, which is enough for the first user's home directory alongside the boot servers. A directory tree of any size waits for the single-process model above, not for a bigger kernel table. If the boot set doesn't fit, `Max_Processes` is raised, and defined once, as a stopgap.

### 3. Users

A user is a process that implements a `User` interface.

```ada
interface User : Named is

   function Home_Directory return interface Writable_Directory;

end User;
```

- **The `user` server** is one process per user. It holds its name and its home directory cap, given to it at launch.
- **No sessions, logins or credentials yet.** Holding a user's cap is being that user, as with any other capability.

### 4. First boot creates monty

Because init runs only on a fresh boot, init creates the first user there:

1. Launch a `directory` process, the home directory.
2. Call `Create_File (".petalrc")` on it, and write `echo Welcome to Rose` and a line break to the new file.
3. Launch a `user` process named `monty`, giving it the home directory.
4. Launch petal with monty's `User` cap and the home directory, alongside the caps it gets now.

Init keeps launching petal for now. Launching a shell for a user is a later step, for example a `User` operation.

### 5. Petal starts with a user and a directory

- **Petal's caps.** As ADR 0002 section 1 says, the user and the directory are roles, `User` and `Home`, that petal declares. Until petal is launched by invocation, they are new entries in the positional cap set, replacing the null `current_directory` slot.
- **The directory is `home`.** It is the first entry in petal's table of named directory caps (ADR 0002, section 5).
- **`.petalrc`.** After printing its version, petal looks up `.petalrc` in its directory. If it is there, petal reads it a line at a time and runs each line through the normal parser and executor, as if typed but without echoing it or showing a prompt. An error in one line is reported and the next line still runs. If there is no `.petalrc`, petal says nothing. Then petal shows its prompt and continues as now.

On first boot, the console therefore shows:

```
Rose petal, version ...
Welcome to Rose
>
```

### 6. The `list` command

```ada
command Rose.Core.List : Standard_Output
  with Summary     => "Write the name of each entry in a directory.",
       Description => """
          List writes the name of each entry in the directory to
          standard output, one to a line, in the directory's order.
          """
is
   Directory : in Directory
     with Help => "the directory to list";
end Rose.Core.List;
```

- **A read-only grant.** `in Directory` means `list` receives a lookup-only cap, narrowed by petal from the writable one it holds (section 1). It can't create or change anything.
- **The shell binds the argument.** A word given for an interface-typed argument is looked up in petal's table of named directory caps, so `list home` lists the home directory. Paths such as `home/src` come later.
- **The default directory is the shell's.** When the argument is omitted, petal binds it to `home`. This is a shell rule for `in Directory` arguments with no default, not a default written in the IDL.
- **Installed with the core utilities**, like `echo`, so it is in the registry before the first prompt.

## Consequences

**Benefits**
- Rose has its first writable, persistent files, with no disk format to design.
- The type of a directory or file cap says whether it can be written, so a command's signature shows it.
- A command such as `list` is given exactly one read-only directory, and nothing else.
- Petal has a per-user start-up file, and the first boot greets the user through it, using an external command.

**Costs**
- **Process count.** Until a directory and its children can share a kernel process, every file and directory uses a kernel process entry, which limits how many files Rose can hold.
- **Breaking change.** isofs, `Rose.Directories` and any other user of `Directory` and `File` must change.
- **Every file operation is IPC** to the file's own process, and creating a file launches a process.
- **Init grows.** It creates a directory, a file and a user on first boot.

## Open questions

- **Read positions.** A file process has one read position, so two holders of the same file cap share it. Should `Read_File` and `Get_Ordinary_File` return a separate reader each time, served by the same process?
- **Large files.** A file's contents live in its process's heap. What limit does that put on file size, and does a file process need its own segment for contents?
- **Measuring the overhead.** Compare a file process's memory with its contents, using the numbers from the observability work (#195, #198). This is a baseline for the single-process model.
- **Delete and kill.** Deleting an entry should end its process and free its resources (#177).
- **Shell commit.** How do one-process files fit with copy-on-write commit (#141)? A new, unnamed file is a process the directory hasn't been given yet, which may make `out File` simple.
- **Using the user cap.** Petal holds the user cap but this milestone uses only the directory. A prompt with the user's name, or launching a shell for a user, could use it.

## Follow-up work

- Split `Directory` and `File` into read-only and read-write interfaces, with parent-interface conversion in the generated clients, and move isofs and `Rose.Directories` to them (#139).
- Later: user-mode scheduling and private process tables, so a directory and its children can run in a single kernel process.
- A `file` server: one process per file, contents in memory (#215).
- A `directory` server: one process per directory, launching file and directory processes on creation (#216).
- A `User` interface and a `user` server (#217).
- Init: on first boot, create monty's home directory with `.petalrc`, launch monty, and launch petal with monty's user and home directory (#218).
- Petal: take a user and directory, keep `home` in its directory table, and run `.petalrc` before the first prompt (#219).
- Petal: bind interface-typed command arguments from its directory table, defaulting to `home` (#220).
- `Rose.Core.List`, installed with the core utilities (#221).
