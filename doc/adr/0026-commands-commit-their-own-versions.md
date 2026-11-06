# 26. Commands commit their own versions

- **Status:** Proposed
- **Date:** 2026-11-05
- **Related:** [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (argument modes, writable interfaces and transactions, which this amends), [ADR 0027](0027-new-files-in-a-transaction.md) (new files in a transaction), [ADR 0010](0010-resources.md) (resources), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), #141 (shell-owned commit), #735 (an editor, ported from kilo), #752 (read-only and writable files), #634 (directory versions), #338 and #340 (redirection)
- **Amended by:** #338, where petal commits a redirection, which is its own and not the command's (section 3).

## Context

ADR 0012, section 3 makes the command the transaction. Petal gives a transactional command a version of each `Versioned` object it names, commits them all if the command completes, and discards them if it fails. #141 implemented this for files: petal holds each version's `File_Version`, and gives the command only its `Writable_File`, so that the command can't commit or discard what it was given.

An editor doesn't fit:

- **An editor saves more than once.** Each save is a commit. Petal commits once, when the command ends, so a save before then isn't a save.
- **Quitting without saving isn't a failure.** To keep the file as it was, the editor would have to fail on purpose, so that petal discards the version. A command shouldn't fail in order to say "don't keep this".
- **Petal empties the version.** A version starts as a copy of the file, but petal truncates it, because `copy` replaces its target (ADR 0012, section 4). An editor needs the contents.

All three come from the same place: petal decides when a command's changes are done, and only the command knows. `copy` is done when it has written the last byte. An editor is done each time the user saves. Petal can only guess, and its guess (commit on success, discard on failure, start empty) is right for `copy` and wrong for an editor.

#141 also made the directory part of each version. `Writable_Directory.New_File_Version` makes the version, the directory serves its `File_Version`, and committing a version of a new name adds the name to the directory. A file's contents are the file's concern, and its name is the directory's, but committing the one went through the other.

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
- **Nothing is committed unless the command commits it.** A command that ends without committing leaves its objects as they were at its last commit, or as they were before it ran.

### 2. A version of a file is the file's

```ada
interface Transactional_Writable_File : Writable_File, Transactional, Resource is
end Transactional_Writable_File;
```

`Writable_File` gains:

```ada
   function New_Version return interface Transactional_Writable_File;
   --  A version of the file, which starts as a copy of it. Writing to the
   --  version doesn't change the file until the version is committed. A
   --  file has at most one version at a time, and this fails if it
   --  already has one.
```

- **It is named for what it adds** to `Writable_File`, as #725 names the readers, and not for how the file server makes it. A version of a directory would be a `Transactional_Writable_Directory` (#634).
- **The file's process serves its versions.** `Commit`, `Rollback` and `Release` change only the file's contents, and need nothing from the directory. If the file is deleted, the directory ends its process, and every call on the version fails.
- **Making a version gives no new right.** Whoever holds a `Writable_File` can already change the file as it likes. A version only lets them change it all at once.
- **A version is a `Writable_File`,** so a command reads and writes it as it would the file. It inherits `New_Version`, which fails on a version: versions don't nest, until ADR 0027 decides whether they should.
- **It is a `Resource`** (ADR 0010). `Release` ends the version and drops what wasn't committed. After it, every cap to the version fails, whoever holds it, and the file can have another version.
- **The directory takes no part.** `Writable_Directory.New_File_Version` and `File_Version` go, and so do `File_Control.New_Version`, `Commit_Version` and `Discard_Version`.

### 3. The argument's type chooses the file or a version

| Argument | Petal binds |
|---|---|
| `in File` | The file, read-only. |
| `in Writable_File` | The file itself. Writes change it as they are made, and it must exist. |
| `in Transactional_Writable_File` | A version of the file. The command commits it. |

- **Petal binds the name first.** For a `Transactional_Writable_File`, petal finds the file named, or creates it with `Create_File` if there is none, and then asks the file for a version. The name is the directory's, and is settled before the command runs.
- **Petal never commits a command's versions.** After the command ends, whether it completed or failed, petal releases each version it made for the command's arguments. That drops what the command didn't commit, and frees the file for another version.
- **Petal commits a redirection,** because the redirection is petal's (#338, #340). The command sees only a `Writer` as its standard output or standard error, so it has nothing to commit. Petal knows when the output is done: when the command ends, and whether it completed. So petal makes a version of the file, passes the command a writer of the version (`Rewrite` for `>`, `Append` for `>>`; ADR 0009, section 4), commits the version if the command completed, and releases it either way. This is the same rule as for arguments: whoever knows when the changes are done commits them.
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

A copy that fails raises before it commits, and petal's release leaves an existing target as it was. A new target is left empty (see Consequences).

The editor (#735) takes a version too. Saving writes the buffer to the version and commits it. Quitting without saving ends the command, and petal's release drops whatever wasn't saved.

### 5. Changes to ADR 0012

This ADR replaces these parts of ADR 0012. ADR 0012 points here:

- **section 2:** the `Versioned` aspect, and the reasons it gives against an interface, in particular that a command given a version could commit or discard it itself. A command given a `Transactional` object may commit it, since that is the right its type gives;
- **section 3:** the command as the transaction, the `Live` aspect, and petal's commit and discard;
- **section 4:** a `Writable_File` argument of a transactional command getting a version, the version being emptied, and a new name getting a version that is named when it commits. Petal now creates a new file before it makes the version (section 3). Which name an argument binds to stays as section 4 has it.

Section 1 (the mode is the direction) and the rest of section 2 (the type is the right) stand.

## Consequences

**Benefits**

- The command, which knows when its changes are done, decides when to commit them. Petal doesn't guess.
- An editor saves as often as the user asks, and quitting without saving needs no failure.
- A command that edits a file sees its contents. A command that replaces it says so, with `Truncate`.
- A file's contents and its name are kept apart. A version belongs to the file's process, so a write takes one hop, and the directory has no versions to keep.
- A name that is already taken by something other than a file fails before the command runs, not when it commits.
- The argument's type says everything petal needs: the file or a version, and who commits. The `Live` and `Versioned` aspects go from idle.
- A version's lifetime is a `Resource`'s (ADR 0010), so `File_Version` goes, and so does petal's commit loop.

**Costs**

- **A new file exists before anything is committed to it.** Petal creates it, empty, before the command runs. If the command fails, or ends without committing, the empty file stays, so a failed `copy` to a new name leaves an empty target, and quitting the editor without saving leaves an empty file. A new file should appear only when its contents are committed, and gain its name then: that is ADR 0027.
- **Several objects aren't committed together.** ADR 0012 committed all of a command's versions or none. Each version now commits on its own, so a command that writes two files can leave one written and the other not, if it fails between the two commits. No command writes two files yet.
- **A command that forgets to commit loses its work** without a message. Petal could warn when it releases a version that was written to and never committed.
- **The file server** serves the version as a `Transactional_Writable_File`, keeps it open after a commit, and adds `Rollback`.
- **The directory server** loses `New_File_Version` and its version objects.
- **Idle:** the `Live` and `Versioned` aspects go, and the signature records the arguments whose type inherits `Transactional`.
- **Petal:** it creates a new file and asks it for a version, stops truncating, releases each version after the command, and loses `Commit_Versions` and the check that refuses a transactional command with a `Versioned` directory argument.
- **`copy`** changes its target's type, truncates it and commits.

## Open questions

- **New files (ADR 0027).** How a new file can stay unnamed until its contents are committed, without the file's commit depending on the directory.
- **Committing several objects together.** A transaction that spans objects could be a separate object, given to the command, that commits every version made for it. ADR 0027's enclosing transaction is one form of this.
- **A default commit.** A command such as `copy` commits once, at the end. Idle could generate that commit when the main returns, for commands that ask for it, so that such commands can't forget. It isn't worth the aspect until there are more of them.
- **Directory versions (#634).** A `Transactional_Writable_Directory` would inherit `Transactional` too. What committing it does to the files inside it is still part of #634 and ADR 0027.
- **Read-only files (#752).** A command can't take a file that may be read-only. That is a question about the type of an argument, and doesn't change what a version is.

## Follow-up work

- Interfaces: add `Transactional` and `Transactional_Writable_File`, add `Writable_File.New_Version`, and remove `File_Version`, `Writable_Directory.New_File_Version`, and `File_Control.New_Version`, `Commit_Version` and `Discard_Version`.
- File server: serve versions as `Transactional_Writable_File`, with a commit that keeps the version open, `Rollback` and `Release`.
- Directory server: remove `New_File_Version` and its version objects.
- Idle: remove `Live` and `Versioned`, and record `Transactional` arguments in the signature (blancolioni/idle#78, #79).
- Petal: bind `in Transactional_Writable_File` by creating the file if needed and asking it for a version, stop truncating, and release versions after the command.
- `copy`: take a `Transactional_Writable_File`, truncate it and commit.
- Point ADR 0012's sections 2, 3 and 4 here.
- The editor (#735), on `Transactional_Writable_File`.
