# 27. New files in a transaction

- **Status:** Proposed. This ADR records an open question and the direction it leans towards; it decides nothing yet.
- **Date:** 2026-11-05
- **Related:** [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (argument modes, writable interfaces and transactions), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), #141 (shell-owned commit), #634 (directory versions)

## Context

ADR 0026 gives a version of a file to the file's process. `Commit`, `Rollback` and `Release` change only the file's contents, and the directory takes no part. Petal binds a name before the command runs: it finds the file, or creates it with `Create_File`, and then asks the file for a version.

That leaves a disconnect for a new file:

- **It exists before anything is committed to it.** Petal creates it empty, so it is listed, and can be read, while the command is still writing its version.
- **It stays if nothing is committed.** A failed `copy` to a new name leaves an empty target. Quitting the editor without saving leaves an empty file.
- **It has a name before it has contents.** A new file should not exist until its contents are committed, and should gain its directory entry at that point, not before.

#141 avoided this by making the directory part of every version: the directory served the version of a new name, and its first commit added the entry. That made the file's commit depend on the directory, which is what ADR 0026 removes. A file's contents are the file's concern, and its name is the directory's.

## The direction: a nested commit

Adding a name and committing a file's contents are two transactions, one inside the other:

1. **Start a directory transaction** with the name and a new file.
2. **Write to the file,** through a version of it.
3. **Commit the file.** It isn't really committed yet, because the directory transaction encloses it.
4. **Commit the directory transaction.** The entry is added, and now everything is committed.

A rollback of the directory transaction rolls back both: the file's contents and the directory entry.

### A new file is the easy case

Nobody can reach a new file until it has an entry: the only caps to it are the ones the transaction handed out. So for a new file, the inner commit can be a real commit of the file's contents, and the outer commit is what makes them visible, by adding the entry. A rollback of the outer transaction ends the unnamed file's process. The file's process needs nothing from the directory, and the directory needs nothing from the file except its caps, which it holds anyway, as it does after `Create_File`.

### A sketch

```ada
interface Directory_Transaction : Transactional, Resource is

   function New_File
     (Name : String)
     return interface Transactional_Writable_File;
   --  A version of a new, empty file, to be called Name when the
   --  transaction commits.

end Directory_Transaction;
```

`Writable_Directory` would gain `function Start_Transaction return interface Directory_Transaction`. `Commit` adds the entries of the transaction's new files, and fails by raising if a name was taken in the meantime. `Rollback` and `Release` end the processes of the new files and add nothing. Whether this is a separate interface, or what a `Transactional_Writable_Directory` (#634) is, is one of the questions below.

## Open questions

- **Who commits the directory transaction?** The command commits its file, but the name was petal's to bind (ADR 0026, section 3).
  - **Petal commits it** after the command, if the file inside it was ever committed. That is not a guess about the command's intent: the file was committed, so it should have a name. But it is petal committing again, which ADR 0026 moved away from.
  - **The command is given the directory transaction,** and commits both. That puts a directory in the signature of every command that may create a file, and the command has to know whether its file was new.
  - **The file's first commit commits the enclosing transaction,** when that transaction holds only the one file. That is the coupling ADR 0026 removed, by another route.
- **What does an enclosed commit mean for an existing file?** A file that already has an entry can be reached by anyone who holds a cap to it. A commit inside an enclosing transaction can't take effect then without being seen early. The file would have to hold its commits back until the enclosing transaction commits, so the file would need to know its parent, and the parent would need to tell it. A new file needs none of this.
- **Is a directory transaction a directory version (#634)?** A version of a directory would hold its entries as they will be after a commit. A transaction that only adds new files is a much smaller thing, and might be enough for petal.
- **Several files committed together.** A directory transaction that encloses several files' commits would commit them all or none, which ADR 0026 gave up. Whether that extends to files in more than one directory is a further question.
- **Rename and delete inside a transaction.** Replacing a file under a new name, or deleting one as part of a larger change, would want the same enclosing transaction. Is that in scope, or only new files?
- **The empty file in the meantime.** Until this is decided, petal creates a new file before the command runs, and an empty file is left when nothing is committed (ADR 0026, Consequences). Petal could remove a file it created if it is still empty after the command, though that would also remove a file that a command committed empty on purpose.

## Alternatives considered

- **A pending entry that the file's first commit adds,** as #141 had it. The file's commit then depends on the directory. ADR 0026 removed this.
- **A hidden entry,** made when the version is made and shown on the first commit. The name is reserved early, which is a choice of its own, and the directory still has to learn of the file's commit.
- **Create early, and remove the file afterwards if it is still empty.** No new interfaces, but it is petal guessing again, and it removes a file that was committed empty on purpose.

## Consequences

None yet. If the direction is taken, a later revision of this ADR records the decision, and ADR 0026's interim behaviour for new files is replaced.
