# 40. Package state: a private directory for each package, in the user's home

- **Status:** Proposed
- **Date:** 2026-11-22
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands and directory caps, section 5), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (argument modes), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), [ADR 0029](0029-installed-capabilities.md) (installed capabilities, section 4), [ADR 0035](0035-system-directory-and-special-files.md) (the system directory and special files), [ADR 0036](0036-directory-views.md) (directory views), #729 (configuration as a `Tree`), #929 (the null stream), #1044 (the role), #1045 (the null directory), #1039 (Z-code interpreter), #1037 (`scheme`)

## Context

Many programs keep something for the user between runs: a Z-code interpreter's saved games (#1039), a REPL's history, an editor's settings, a cache. On Unix they write it wherever they like under `$HOME`, usually in a dot-directory, with the user's full authority over the rest of the home directory.

Rose has no ambient authority, so a program can only keep something if it is given somewhere to keep it. ADR 0029, section 4, says where that is *not*: per-user state is never installed, because it is writable and belongs to the user. It says that such state is "an ordinary argument, which petal can bind from the user's named directories", but nothing says which directory, or how petal knows.

This is separate from persistence. Persistence means a running process continues across restarts and upgrades: the game being played is still there. Package state is for state that a program writes on purpose so that a *later, separate* process can read it back: a saved game to restore after the first process has ended, a setting that every future run should see.

It is also separate from the state that a Saveable process hands to its replacement (ADR 0032, section 10). That state moves from one process to the next, inside a handover. Package state is files, written by the program in its own format, that stay in the user's home whether or not any process of the package is running.

### What exists today

- **Petal holds `home` as a `Writable_Directory`** (ADR 0003, section 5) and binds roles from the caps it holds. A role it can't supply gets the null cap, and the command must cope (the `Storage_Service` role did this until #517).
- **A command's signature names its package** (`Package_Name` in the metadata), so petal knows it before launch.
- **No directory can reach its parent.** A cap for `home/x` gives `x` and what is below it, nothing else.
- **A null stream is planned** in the system directory (ADR 0035, #929). There is no null directory.

## Decision

### 1. A `Package_State` role

```ada
role Files.Package_State : Files.Writable_Directory;
```

A command that keeps data between runs declares the role:

```ada
command Zcode.Play : Standard_IO, Files.Package_State
is
   Story : in File;
end Zcode.Play;
```

- **It's a public role, with no category** (ADR 0021). It gives the command part of the user's own data, and nothing the user doesn't already have, so the rights broker isn't involved.
- **Its directory belongs to the package,** not the command. Every command of the package gets the same directory, so a `Zcode.Play` and a later `Zcode.Inspect` see the same saves, and so do later versions of the package.
- **It is visible.** `--help` shows the role and what it is bound to, for example `Package_State (home/package-state/zcode)`, which keeps ADR 0029's rule that nothing a command can reach is hidden.
- **It is a directory grant.** The command can create, rename and delete what is in it, and nothing outside it, because no directory reaches its parent.

### 2. Where it lives: `home/package-state/<package>`

- **It's in the user's home,** so the user can see it, copy it, back it up and delete it with the tools they use for everything else. Nothing about it is special to the file system.
- **The name is the package's name, in the file naming convention:** lower case, with `-` for `.`, so the package `Games.Zcode` has `home/package-state/games-zcode`.
- **The parent directory is called `package-state`.** The prefix says whose it is, and `state` says what it holds better than `data`, which could be anything.
- **Its name isn't hidden.** Nothing in Rose treats a leading `.` as special. Unix hides dot-files only because its first `ls` skipped names starting with `.`, which is not a reason to copy it. If a directory should be left out of listings, that is a property of the directory, not a rule about its name (see the open questions).

### 3. Petal binds it, and makes it the first time

- **Petal makes the directory on the first launch that needs it,** in `home` through its `Writable_Directory`: `package-state` if there isn't one, then the package's directory. It commits both straight away (ADR 0026), so they exist even if the command fails.
- **Petal narrows it to the role's type and hands it to the command.** Like every argument, it crosses the job's membrane (ADR 0002, section 9), and the command's hold on it ends with the job.
- **The user can bind something else.** A test, or a fresh start, can give the role another directory, as for any argument (see the open questions for how a role is named on the command line).

### 4. Every launcher supplies it: a package directory or the null directory

A command never receives the null cap for `Package_State`, so it never has to check for one.

- **A launcher that holds the user's home** binds the package's directory, as petal does.
- **A launcher that doesn't,** such as a program that runs a helper, or init starting a server, gives the **null directory** (section 5), or a directory of its own choosing that it holds, such as a subdirectory of its own `Package_State`.
- **A program that launches another command of its own package** can pass its own `Package_State` on.

### 5. The null directory

A `Writable_Directory` with nothing in it, which refuses every change:

- **It lists as empty.** `Directory_Entry_Count` is 0, and every lookup fails as it does for a name that isn't there.
- **Every change fails by raising:** `Create_File`, `Create_Directory`, `Rename`, `Delete_File`, `Delete_Directory` and `Add_Special_File`.
- **A program can tell.** A Z-code `save` given the null directory fails, and the interpreter says that there is nowhere to save. It doesn't appear to work and then lose the save.
- **It is served by the same server as the null stream** (#929), and is in the system directory beside it, as `null-directory`.
- **It is the standard thing to give for any directory argument with nothing behind it,** not only this role.

### 6. Configuration

A program's settings can be kept in its package state. When `Directory` inherits `Tree` (#729, #730), a program reads them through `Tree`, so configuration that isn't a file system can stand in for the directory.

**`settings` is reserved** (ADR 0042, section 7). In every package's state directory, the `settings` directory belongs to the `Rose.Configuration` library, which keeps the user's overrides of the package's installed defaults there, one value to a file. The rest of the package's state directory is the program's own.

### 7. Uninstalling

The uninstaller asks whether to keep each user's package state or remove it. It keeps it by default, so that reinstalling the package finds it again.

## Consequences

- **A program that saves state needs no argument for it,** and the user doesn't have to say where saves go on every run.
- **The user owns the data,** in plain view, under their home, and its storage is charged to their bank (ADR 0014).
- **Every package's data is separate** from every other's, and a command can never see the rest of the home directory through it.
- **The package name becomes a boundary.** Whatever can install a package under a name can read and write that name's data for every user who runs it. Installing under an existing name has to be as tightly held as replacing that package (ADR 0031).
- **Two processes of a package can write the same directory at once.** `Writable_Directory` is versioned (ADR 0012), so each commits its own versions (ADR 0026), and a conflict is the package's to handle, as with any shared directory.
- **Data formats are the package's concern** across its own versions, as on any system.
- **Idle and petal:** the role, petal's binding and its directory creation, and `--help`.
- **The null directory** needs the system directory and its server (#929).

## Alternatives considered

- **A separate directory cap beside `home`,** given to petal at login. It keeps package state out of the user's listing, but it's another cap in every session, and it hides the user's own data from them.
- **An ordinary argument** that the user passes every time. That puts work on every run of every such program, which is what this ADR removes.
- **One directory per command,** not per package. Commands of one package couldn't share saves, and renaming a command would lose them.
- **`home/.local/<package>`,** after the Unix and XDG convention. The leading dot hides it for no reason that applies to Rose (section 2).
- **A null directory that accepts and discards changes,** like `/dev/null` for streams. A program would think it had saved, and find nothing when it restored. A discarding directory could still be added later for programs that write things nobody needs.
- **The null cap,** as petal gives for roles it can't supply. Every program would have to check for it, and get it wrong when it didn't.
- **Other names for the directory:** `local`, which says only where the data is, and will also read as "not remote" once there is networking (ADR 0007); `package-data`, where `data` could be anything; and `programs`, `kept` and `saved`.

## Open questions

- **Leaving directories out of listings.** If `package-state` clutters `list`, should a directory be able to carry a "not listed by default" property?
- **Naming a role on the command line.** Arguments can be overridden by name (ADR 0029, section 2). A role has no argument name yet. One option is `--role Package_State=scratch/test`.
- **Quotas.** The data is charged to the user's bank. Should a package's directory be able to have a limit of its own?

## Follow-up work

- The `Files.Package_State` role (#1044), declared before the 0.4.0 tag, since the interfaces freeze with it (ADR 0031). Petal's binding (creating the directories, and showing it in `--help`) can follow in 0.4.x.
- The null directory (#1045), with the null stream (#929).
- The uninstaller's question (section 7).
- #1039: the Z-code interpreter saves games to its package state, in the Quetzal format.
