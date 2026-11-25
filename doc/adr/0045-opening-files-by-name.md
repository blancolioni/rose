# 45. Opening files by name from Ada.Text_IO

- **Status:** Proposed
- **Date:** 2026-11-25
- **Related:** #1124 (this decision), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands and directory caps), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (rights are types), [ADR 0021](0021-privilege.md) (categorised roles), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), [ADR 0027](0027-new-files-in-a-transaction.md) (new files in a transaction), [ADR 0036](0036-directory-views.md) (directory views), [ADR 0044](0044-petal-paths-as-uri-references.md) (paths as URI references)
- **Amends:** [ADR 0044](0044-petal-paths-as-uri-references.md), whose consequences say that petal is the only program that resolves paths

## Context

The userland run-time system has three streams, standard input, output and error, and nothing else. Its `fopen` and `freopen` set `ENOENT` and return a null stream, so `Ada.Text_IO.Open` and `Create` on a named file raise `Name_Error`. `Sequential_IO`, `Direct_IO`, `Stream_IO` and `Ada.Directories` aren't in the run-time system at all.

A command that works with files is given caps for them as arguments (ADR 0002), and petal resolves the names the user types (ADR 0044). That still suits most commands. It doesn't suit programs whose file names come from somewhere other than the command line, such as a configuration file, a project file, a file that names other files, or a library written for an Ada with files. These need to open a file by name, which `Ada.Text_IO` already provides an interface for.

Opening by name has to keep what Rose has now: there is no global namespace, there is no "up" from a directory cap, and a command gets the authority it declares and no more.

## Decision

### 1. A name is a path, as petal reads it

The `Name` given to `Ada.Text_IO.Open` or `Create` is a URI reference, with ADR 0044's rules:

- **`//name/path`** starts from the named directory `name` (section 4).
- **Any other path is relative**, and starts from the command's current directory (section 2).
- **`/path` is refused.** The run-time system has the current directory, not the root it is in.
- **`..` is refused wherever it would leave the current directory.** A directory cap has no parent, and the current directory is all the command was given.
- **Empty segments, unknown names and schemes are refused,** as ADR 0044, section 5, refuses them.

Each refusal raises `Name_Error`.

### 2. The current directory is a role, one for each view

```ada
role Files.Current_Directory : Files.Directory;
role Files.Current_Fixed_Directory : Files.Fixed_Directory;
role Files.Current_Writable_Directory : Files.Writable_Directory;
```

- **The role a command declares is the right it asks for** (ADR 0012, section 2), and it shows in the command's IDL. A command that only reads declares `Current_Directory`.
- **Petal grants its current directory, narrowed to the role's view** (ADR 0036). Narrowing is free.
- **Petal refuses to launch a command with `Current_Writable_Directory`** when the current directory is fixed, as under `//system`, just as it refuses a `Writable_Directory` argument there (ADR 0035, section 2).
- **A command declares one of the three.**

### 3. Named directories, read-only

```ada
interface Files.Directory_Names is
   function Get_Directory (Name : String) return interface Files.Directory;
   --  The named directory, as a read-only view. Fails if there is no
   --  directory called Name.
end Files.Directory_Names;

role Files.Named_Directories : Files.Directory_Names;
```

- **Petal serves it from its table:** `//home`, `//system`, and whatever the table holds later.
- **It always returns a `Directory`,** which is read-only all the way down (ADR 0036, section 1), even for a name that petal holds writable. So the role gives a command read access to the table, not write access. To write outside its current directory, a command still needs an argument.
- **It can become a categorised role later** (ADR 0021), so that the rights broker can refuse it, without changing the interface.

### 4. The run-time system's table of names

The run-time system keeps a table of named directory caps:

- **`//name` looks in the table first, then asks `Named_Directories`,** if the command has that role. If neither has `name`, `Name_Error`.
- **A program can add to the table,** for example a `Directory` argument under a name of its own choosing, so that names it reads elsewhere can refer to it.
- **The current directory isn't in the table.** Relative paths reach it, and it has no name of its own.

### 5. What Text_IO does

| Operation | Needs | Does |
|---|---|---|
| `Open`, `In_File` | any view | walks to the file, then `Open_Reader`. A special file gives its reader. |
| `Open`, `Out_File` | `Fixed_Directory` or wider | `Get_Writable_File`, `New_Version`, `Rewrite` |
| `Open`, `Append_File` | `Fixed_Directory` or wider | `Get_Writable_File`, `New_Version`, `Append` |
| `Create`, existing file | `Fixed_Directory` or wider | as `Out_File` |
| `Create`, new name | `Writable_Directory` | `Create_File`, then as `Out_File` |
| `Close` | | commits the version, then releases it |

- **The program commits on `Close`,** as ADR 0026 has commands commit their own versions. Each file is its own transaction.
- **A file still open when the program ends is released without a commit,** including when it ends with an exception. GNAT's finalization closes open files, so the run-time system tells that close from the program's own `Close`, and only the program's own commits. A program that fails part way leaves the files it was writing as they were.
- **`Create` of a new name makes the entry at once,** with `Create_File`, so a program that fails leaves it empty. This is ADR 0027's open question, and this ADR follows whatever it decides.
- **Rights are checked by the type of the cap the run-time system holds,** with a membership test, not by asking the server. A missing right raises `Use_Error`.
- **`Delete`, and `Form` strings, aren't supported:** `Delete` raises `Use_Error`, and `Form` is ignored.

### 6. Roles reach the run-time system generically

Idle knows the names of the standard-stream roles, and the generated main hands their caps to the run-time system one by one. Adding the roles of sections 2 and 3 the same way would mean changing idle for each new role.

Instead, the generated main hands every role member it gets to the run-time system, with the member's name. The run-time system keeps the ones it knows and ignores the rest. This is one change to idle, after which new roles need none.

### 7. Scope

- **`Ada.Text_IO` only.** `Sequential_IO` and `Stream_IO` can follow on the same `fopen`, by adding them to the run-time system's GNAT units. `Direct_IO` needs writes at a position, which no file interface has yet. `Ada.Directories` is separate work.
- **Committing a file that stays open,** for something like a log, is left for later. `Common.Transactional.Commit` already leaves the object open, so it would be a run-time system procedure rather than an interface change. A database will often suit that need better.

## Consequences

- **Programs can open files by name,** with ADR 0044's paths, and libraries that use `Ada.Text_IO` can be ported.
- **Authority stays visible.** What a command can open is its roles and arguments, which its IDL shows. The names role reads the table and never writes it.
- **Petal is no longer the only resolver of paths.** The run-time system follows the same rules, so a path means the same thing in both, except that it can't go above the current directory.
- **A failed program leaves no half-written file,** but it can leave a new, empty one.
- **New interfaces and roles only,** which fits the interface rules from 0.4.0. Planned for 0.5.0.

## Alternatives considered

- **`Named_Directories` returns what petal holds,** writable for `home`. This gives a command write access to everything in the table without an argument, which is the ambient authority ADR 0002 avoids.
- **Petal gives the root and the path within it,** so that `/path` and `..` work as they do in petal. This widens every current-directory grant to its whole root.
- **Commit at finalization.** A program that fails part way would commit what it had written so far.
- **The shell commits when the command completes,** as it does for a redirection (ADR 0026, section 3). The run-time system would have to hand its versions to petal, which needs another interface. ADR 0026 also explains why petal can't tell when a program's changes are done.
- **One current-directory role, typed `Files.Directory`,** with the program testing what it got. The right a command asks for wouldn't show in its IDL.

## Implementation

1. IDL: the three current-directory roles, `Files.Directory_Names` and `Files.Named_Directories`.
2. Idle: the generated main hands every role member to the run-time system by name (section 6).
3. Petal: grant the current-directory roles and serve `Directory_Names`.
4. The run-time system: the path resolver, the table of names, and `fopen`, `fclose`, reading and writing for opened files in `Interfaces.C_Streams`, with commit on `Close` and release at finalization.
5. A test command that opens, writes, closes and reads back a file, and one that fails part way, in the boot test.
6. The Rose Book: `files.adoc`, and what `programs.adoc` says about the run-time system.
