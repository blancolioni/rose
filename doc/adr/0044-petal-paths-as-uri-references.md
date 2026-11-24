# 44. Petal's paths are URI references

- **Status:** Proposed
- **Date:** 2026-11-24
- **Related:** #1096 (this decision), #342 (the current directory), #343 (`pwd`), #929 (the system directory), [ADR 0002](0002-typed-commands-and-directory-caps.md) (directories as named caps), [ADR 0035](0035-system-directory-and-special-files.md) (the system directory and special files), [ADR 0036](0036-directory-views.md) (directory views)
- **Amends:** [ADR 0002](0002-typed-commands-and-directory-caps.md), section 5 (how a path names a directory in the table), and [ADR 0035](0035-system-directory-and-special-files.md), sections 2 and 5 (how petal names `system` and its special files)

## Context

Petal keeps a table of named directory caps instead of a root (ADR 0002, section 5). Today it has `home`, from the user's `User` interface, and `system`, from login (ADR 0035, ADR 0036).

ADR 0002 says that a path's first component names an entry in the table, as in `projects/rose/README`, and ADR 0035 follows it with `system/null`. Both were written before petal had a current directory. Since #342, a path starts from the current directory, so `cd` needed another way to name a table entry, and brought in `[root]`: `cd [home]/src/rose`. `pwd` writes the same form (#343).

The rest of petal still follows ADR 0002:

| Where | How a named directory is written |
|---|---|
| `cd`, `pwd` | `[home]/notes`; a bare path starts from the current directory |
| `in Directory` arguments | a table name only: `home`, or `system` |
| readers and redirections | `system/null`, matched as a prefix of the word, for a special file |
| `in File`, versions | a bare path from the current directory |

So `system/null` names the special file even where the current directory has a subdirectory called `system`, and `home/notes` typed in `[home]` looks for a subdirectory called `home`. `system` isn't in the table, so `cd [system]` fails. A directory argument can't be a path. What `pwd` writes can't be typed back as an argument.

## Decision

### 1. One rule for every path

Every path that petal reads is a URI reference (RFC 3986), resolved against the current directory:

- **`//name/path`** starts from the named directory `name`, which is the reference's authority: `//home/docs/notes`, `//system/null`.
- **`/path`** starts from the root of the named directory that the current directory is in, as RFC 3986 resolves an absolute path against a base.
- **Any other path is relative,** and starts from the current directory: `notes`, `docs/notes`, `../notes`.

The same rule applies to `cd`, to directory arguments, to `in File` and `in Writable_File` arguments, to readers, to versions and to redirections. A special file is found by looking its path up like any other: `cat //system/null` reads it, and `echo hi > //system/null` writes to it (ADR 0035, section 5).

### 2. A directory argument is a path

An `in Directory` or `in Writable_Directory` argument is resolved as a path, not only as a table name. `list --directory=//home/docs` and `list --directory=docs` both work. With no argument, a command still gets the current directory (#342).

### 3. `system` is in the table

`system` becomes an entry in petal's table, next to `home`, so `//system` works wherever `//home` does, and `cd //system` makes it the current directory. Petal still never grants it as a `Writable_Directory` (ADR 0035, section 2), and holds it as a `Fixed_Directory` (ADR 0036). This amends ADR 0035, section 2, which named `system` but kept it outside the table.

### 4. `pwd` and `cd`

- `pwd` writes the current directory as `//name/path`, which petal accepts anywhere a path is.
- `cd` with no argument goes to `//home`.
- `[root]` is no longer accepted.

### 5. Errors rather than guesses

- **`..` above a named directory is an error,** as it is today. RFC 3986 removes dot segments that would climb above the root without saying so. Petal refuses instead, because a path someone typed that climbs out of its root is a mistake, and there is nothing above the root to reach (ADR 0002, section 5).
- **An empty segment is an error.** Only a leading `//` has a meaning. `docs//notes` is refused, not read as `docs/notes`, so a typo shows.
- **An unknown authority is an error:** `//nothing/x` says that there is no directory called `nothing`.
- **A word with a `:` before its first `/` is refused,** as a scheme that petal doesn't know. Schemes are reserved (section 6), and refusing them now means that adding one later can't change what an existing path means.

### 6. Names

- **Authorities are the table's names,** compared as entry names are, so they are case-sensitive.
- **A name with a space in it is quoted,** as petal already quotes words. Percent-encoding isn't used, since petal never takes a path from anywhere that needs it.
- **Schemes are for later.** `scheme://authority/path` is the general form of a URI. When Rose has a use for one, such as network names, a scheme would name a cap that petal holds and that resolves the rest, as a table name names a directory cap. Nothing in this ADR depends on it.

## Consequences

- **One rule everywhere,** and the form that `pwd` writes works as an argument.
- **Named and relative paths can't be confused.** A subdirectory called `home` or `system` is just a subdirectory, and adding a name to the table later can't change what an existing relative path means, in a script or in `.petalrc`.
- **The syntax is a standard one,** with resolution rules that are already written down. Petal is stricter than RFC 3986 about `..` and empty segments (section 5).
- **Existing habits change.** `system/null` becomes `//system/null`, and `cd [home]` becomes `cd //home`. Petal is the only program that resolves these paths: a command gets caps for its directory and file arguments, and a command that takes a name as a `String` looks it up in a directory it was given, which this doesn't change.
- **No interface changes,** so this can go into 0.4.1.

## Alternatives considered

- **The first component names the table entry,** as ADR 0002 says. With a current directory, `home/notes` is ambiguous, and adding an entry to the table would change what existing relative paths mean.
- **`[name]/path` everywhere,** extending what `cd` and `pwd` do now. It is unambiguous, but it is syntax petal invented, it has to be typed with brackets, and it doesn't extend to schemes.
- **A sigil for a named directory,** such as `@system/null`. ADR 0035 turned down `@null` as new syntax with a special case for each name, and a sigil for roots has the same cost without the URI form's resolution rules.

## Implementation

1. `Petal.Session`: one resolver for every path, following sections 1 and 5, used by `Change_Directory`, `Lookup_File`, `Open_Reader`, `New_Version`, `Redirect_Output` and the directory grants. `system` goes in the table. `Current_Directory_Name` writes `//name/path`.
2. `Petal.Command_Launch`: directory arguments resolve a path (section 2), and `cd` with no argument goes to `//home`.
3. The Rose Book's `files.adoc`, and the boot test's checks of `system/null` in `scripts/test-boot`, use `//system/null` and `//home`.
