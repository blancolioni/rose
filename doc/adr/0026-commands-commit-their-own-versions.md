# 26. Commands commit their own versions

- **Status:** Proposed
- **Date:** 2026-11-05
- **Related:** [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (argument modes, writable interfaces and transactions, which this amends), [ADR 0010](0010-resources.md) (resources), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), #141 (shell-owned commit), #735 (an editor, ported from kilo), #752 (read-only and writable files), #634 (directory versions)

## Context

ADR 0012, section 3 makes the command the transaction. Petal gives a transactional command a version of each `Versioned` object it names, commits them all if the command completes, and discards them if it fails. #141 implemented this for files: petal holds each version's `File_Version`, and gives the command only its `Writable_File`, so that the command can't commit or discard what it was given.

An editor doesn't fit:

- **An editor saves more than once.** Each save is a commit. Petal commits once, when the command ends, so a save before then isn't a save.
- **Quitting without saving isn't a failure.** To keep the file as it was, the editor would have to fail on purpose, so that petal discards the version. A command shouldn't fail in order to say "don't keep this".
- **Petal empties the version.** A version starts as a copy of the file, but petal truncates it, because `copy` replaces its target (ADR 0012, section 4). An editor needs the contents.

All three come from the same place: petal decides when a command's changes are done, and only the command knows. `copy` is done when it has written the last byte. An editor is done each time the user saves. Petal can only guess, and its guess (commit on success, discard on failure, start empty) is right for `copy` and wrong for an editor.

## Decision

### 1. The `Transactional` interface

```ada
interface Transactional is

   procedure Commit;
   --  Make the changes since the last Commit the object's contents. The
   --  object stays open, for more changes.

   procedure Rollback;
   --  Drop the changes since the last Commit. The object stays open.

end Transactional;
```

An interface whose objects can be committed inherits `Transactional`. Holding one is the right to commit it: the type is the right (ADR 0012, section 2).

- **`Commit` keeps the object open.** After a commit, the object holds the committed contents, and the next changes start from there. An editor commits each time it saves.
- **`Commit` fails by raising** if it can't commit: for a new file, if its name was taken in the meantime, or for any file, if it was deleted. The changes are then still there, to roll back or to try again.
- **Nothing is committed unless the command commits it.** A command that ends without committing leaves its objects as they were at its last commit, or as they were before it ran.

### 2. A version of a file

```ada
interface Transactional_Writable_File : Writable_File, Transactional, Resource is
end Transactional_Writable_File;
```

- **It is named for what it adds** to `Writable_File`, as #725 names the readers, and not for how the file server makes it. A version of a directory would be a `Transactional_Writable_Directory` (#634).
- **It starts as a copy of the file,** or empty for a name with no file yet. A command that replaces the file truncates it first.
- **It is a `Writable_File`,** so a command reads and writes it as it would the file.
- **It is a `Resource`** (ADR 0010). `Release` ends the version and drops what wasn't committed. After it, every cap to the version fails, whoever holds it.
- **`Writable_Directory.New_File_Version` returns it,** in place of `File_Version`, which goes. The first commit of a version of a new name adds the file to the directory, and later commits change that file.

### 3. The argument's type chooses the file or a version

| Argument | Petal binds |
|---|---|
| `in File` | The file, read-only. |
| `in Writable_File` | The file itself. Writes change it as they are made, and it must exist. |
| `in Transactional_Writable_File` | A version of the file, or of a new file if there is none. The command commits it. |

- **Petal never commits.** After the command ends, whether it completed or failed, petal releases each version it made for it. That drops what the command didn't commit, and frees the file for another version.
- **Petal doesn't empty a version.** Whether a command replaces a file, appends to it or edits it is the command's to decide.
- **The `Live` aspect goes.** It chose between the file and a version, and the argument's type now does that. A command that changes a directory as it runs takes a `Writable_Directory`, which is live until directories have versions (#634).
- **The `Versioned` aspect goes.** It marked the interfaces that petal versions. Petal versions an argument whose type inherits `Transactional`, and idle records that in the command's signature.

### 4. Commands

`copy` takes a version, replaces its contents, and commits it when the copy is complete:

```ada
command Rose.Core.Copy : Standard_Error
  with Summary => "Copy a file."
is
   Source : in File
     with Help => "the file to copy";
   Target : in Transactional_Writable_File
     with Help => "the file to write";
end Rose.Core.Copy;
```

A copy that fails raises before it commits, and petal's release leaves the target as it was, or leaves no target. That's the same as before, now said by `copy` and not by petal.

The editor (#735) takes a version too. Saving writes the buffer to the version and commits it. Quitting without saving ends the command, and petal's release drops whatever wasn't saved.

### 5. Changes to ADR 0012

This ADR replaces these parts of ADR 0012. ADR 0012 points here:

- **section 2:** the `Versioned` aspect, and the reasons it gives against an interface, in particular that a command given a version could commit or discard it itself. A command given a `Transactional` object may commit it, since that is the right its type gives;
- **section 3:** the command as the transaction, the `Live` aspect, and petal's commit and discard;
- **section 4:** a `Writable_File` argument of a transactional command getting a version, and the version being emptied. Which name an argument binds to stays as section 4 has it.

Section 1 (the mode is the direction) and the rest of section 2 (the type is the right) stand.

## Consequences

**Benefits**

- The command, which knows when its changes are done, decides when to commit them. Petal doesn't guess.
- An editor saves as often as the user asks, and quitting without saving needs no failure.
- A command that edits a file sees its contents. A command that replaces it says so, with `Truncate`.
- The argument's type says everything petal needs: the file or a version, and who commits. The `Live` and `Versioned` aspects go from idle.
- A version's lifetime is a `Resource`'s (ADR 0010), so `File_Version` goes, and so does petal's commit loop.

**Costs**

- **Several objects aren't committed together.** ADR 0012 committed all of a command's versions or none. Each version now commits on its own, so a command that writes two files can leave one written and the other not, if it fails between the two commits. No command writes two files yet.
- **A command that forgets to commit loses its work** without a message. Petal could warn when it releases a version that was written to and never committed.
- **The file and directory servers change.**
  - The file server keeps the version open after a commit, as a copy of what was committed, and has `Rollback`.
  - The directory server commits a version of a new name by adding the entry once, and then commits to that file.
  - Both serve `Transactional_Writable_File` and stop serving `File_Version`.
- **Idle:** the `Live` and `Versioned` aspects go, and the signature records the arguments whose type inherits `Transactional`.
- **Petal:** `New_Version` stops truncating, `Commit_Versions` goes, a version is released after the command, and the check that refuses a transactional command with a `Versioned` directory argument goes.
- **`copy`** changes its target's type, truncates it and commits.

## Open questions

- **Committing several objects together.** A transaction that spans objects could be a separate object, given to the command, that commits every version made for it. That waits for a command that writes two files.
- **A default commit.** A command such as `copy` commits once, at the end. Idle could generate that commit when the main returns, for commands that ask for it, so that such commands can't forget. It isn't worth the aspect until there are more of them.
- **Directory versions (#634).** A `Transactional_Writable_Directory` would inherit `Transactional` too. What committing it does to the files inside it is still part of #634.
- **Read-only files (#752).** A command can't take a file that may be read-only. That is a question about the type of an argument, and doesn't change what a version is.

## Follow-up work

- Interfaces: add `Transactional` and `Transactional_Writable_File`, return it from `New_File_Version`, and remove `File_Version`, `Commit_Version` and `Discard_Version`.
- File and directory servers: commit that keeps the version open, `Rollback`, and the first commit of a new name.
- Idle: remove `Live` and `Versioned`, and record `Transactional` arguments in the signature (blancolioni/idle, issue to be opened).
- Petal: bind `in Transactional_Writable_File`, stop truncating, and release versions after the command.
- `copy`: take a `Transactional_Writable_File`, truncate it and commit.
- Point ADR 0012's sections 2, 3 and 4 here.
- The editor (#735), on `Transactional_Writable_File`.
