# 36. Directory views: read-only, all the way down or not

- **Status:** Proposed
- **Date:** 2026-11-14
- **Related:** #938 (this decision), #929 (the system directory, which this blocks), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (rights are types), [ADR 0035](0035-system-directory-and-special-files.md) (the system directory and special files, whose section 3 this amends), [ADR 0031](0031-updating-an-installation.md) (the interfaces freeze with 0.4.0)

## Context

A read-only directory can mean either of two things, and both are worth supporting:

1. **The directory can't change, but its entries keep their own rights.** Nobody can add, rename or delete an entry, but a file in it can still be written. A log directory is like this, and so is `system`, where `null` must still take writes.
2. **The directory is read-only all the way down.** Everything reached through it is read-only too. This is the view to give a command that should only look, such as a backup or a search.

### What exists today

- **`Directory` is case 2 for what a directory serves** (ADR 0003, section 1). `Get_Ordinary_File` returns a `File` and `Get_Directory` returns a `Directory`. The writable versions come only from `Writable_Directory`, through `Get_Writable_File` and `Get_Writable_Directory`.
- **ADR 0035, section 3, makes special files case 1 without a type.** It says that a directory's rights are over the directory and not over its entries, so a read-only `system` gives `null` as a `Stream`. Nothing in the type of the `system` cap says that its entries can be written.
- **Nothing is implemented yet.** `Get_Special_File` and `Add_Special_File` from ADR 0035 are not in the IDL yet.

### Constraints

- **Rights are types** (ADR 0012). The type of the cap that a command holds should say what it can do, without reading the documentation of the server that made it.
- **Narrowing is free** (ADR 0003, section 1). An interface cap is a set of endpoint caps, so a parent interface's client is a subset of its child's.
- **An endpoint id is derived from the interface that declares the subprogram** (idle#3). Moving a subprogram to another interface changes its id unless it is pinned with `Endpoint_Id`.
- **The interfaces freeze with 0.4.0** (ADR 0031).

## Decision

### 1. One interface for each case

```ada
interface Directory is
   ...
   function Get_Ordinary_File (Index : Positive) return interface File;
   function Get_Directory (Index : Positive) return interface Directory;
   function Get_Special_Reader (Index : Positive) return interface Reader;
end Directory;

interface Fixed_Directory : Directory is
   function Get_Writable_File
     (Index : Positive) return interface Writable_File;
   function Get_Fixed_Directory
     (Index : Positive) return interface Fixed_Directory;
   function Get_Special_File (Index : Positive) return interface Stream;
end Fixed_Directory;

interface Writable_Directory : Fixed_Directory
  with Versioned
is
   function Get_Writable_Directory
     (Index : Positive) return interface Writable_Directory;
   function Create_File (Name : String) return interface Writable_File;
   function Create_Directory
     (Name : String) return interface Writable_Directory;
   procedure Rename (From : String; To : String);
   procedure Delete_File (Name : String);
   procedure Delete_Directory (Name : String);
   procedure Add_Special_File (Name : String; File : interface Stream);
end Writable_Directory;
```

- **`Directory` is case 2.** Everything it returns is read-only, and a child directory is a `Directory` again, so the view holds all the way down. Of a special file it gives only the `Reader`.
- **`Fixed_Directory` is case 1.** It can't change the directory, but each entry comes back at its own rights: a writable file, the `Stream` of a special file, and a child directory as a `Fixed_Directory`, so the view holds all the way down here too.
- **`Writable_Directory` is the directory and everything in it.** A child directory is a `Writable_Directory`.
- **Each narrows to the one above it for free**, by dropping endpoint caps: `Writable_Directory` to `Fixed_Directory` to `Directory`.
- **The name says what is fixed:** the directory, not its entries.

### 2. Special files follow the type

This replaces ADR 0035, section 3's bullet "A directory's rights are over the directory, not over its entries".

- **A special file's rights are the narrower of the rights it was added with and those of the view it is reached through.** Through a `Directory`, it is a `Reader`. Through a `Fixed_Directory` or a `Writable_Directory`, it is the `Stream` it was added as.
- **`Get_Special_Reader` and `Get_Special_File` fail** for an entry that isn't a special file, as `Get_Ordinary_File` does for one that isn't a file.
- **ADR 0035's `Directory.Get_Special_File` becomes `Fixed_Directory.Get_Special_File`**, and `Directory` gets `Get_Special_Reader` instead.

### 3. Who gets which view

- **Petal gets `system` as a `Fixed_Directory`.** This amends ADR 0035, section 2. `echo hi > system/null` writes, and nobody can change what is in `system`.
- **Petal gets `home` as a `Writable_Directory`**, as now.
- **A command gets the view its argument's type names.** `in Directory` gives it case 2 and `in Fixed_Directory` gives it case 1, whichever directory petal resolves the argument from. A command given `in Directory` from `system` can only read `null`.

### 4. Ids

- **Changing `Writable_Directory`'s parent keeps every id.** The ids of its own subprograms name `Writable_Directory`, and the inherited ones name `Directory`, as before.
- **`Get_Writable_File` moves from `Writable_Directory` to `Fixed_Directory`,** which changes its id. It isn't pinned (amended by #938): this lands before the 0.4.0 tag, when no saved process has to keep working, and #730 (ADR 0042) changes the ids of `Directory` at the same time. A pin would stay in the IDL for good and protect nothing.
- **The new subprograms get new ids.**
- **All of this lands before the 0.4.0 tag.** After that, `Directory` can only be extended or retired.

### 5. Servers

- **The directory server serves all three.** A `Fixed_Directory` client is a subset of its `Writable_Directory` endpoint caps, so the server doesn't need to know which view a client has.
- **isofs serves `Directory` only.** A CD can't be written, and it has no special files, so `Get_Special_Reader` always fails.

### 6. Order of work

1. This ADR, with ADR 0035 marked as amended.
2. The IDL: `Fixed_Directory`, `Get_Special_Reader`, and the directory server and isofs.
3. #929 continues from its step 2, against these interfaces.

### 7. Linking a directory in (#1045)

A directory can hold a directory that it didn't make, as a special file holds a stream it was given. The system directory holds the null directory this way (ADR 0040, section 5).

```ada
interface Linking_Directory : Writable_Directory is
   procedure Add_Directory
     (Name : String; Directory : interface Writable_Directory);
end Linking_Directory;
```

- **It is a right of its own,** and so a type of its own (ADR 0012). A link makes a second name for a directory, and can make a cycle. A program walking a `Tree` can't see a cycle, because a cap carries no identity that it could compare.
- **Only whoever launches a directory has it.** The directory server serves `Linking_Directory` for every directory, but gives it out only as the directory command's `Linking` result. Auth, a directory's parent and the self tests delete it. Init keeps it for the system directory. Everyone else has a directory as a `Writable_Directory` or narrower, so their trees have no links and no cycles.
- **A linked entry is a directory,** and is handed out at the rights of the view it is reached through: through petal's fixed `system`, `null-directory` is a `Fixed_Directory`.
- **Deleting it removes the entry and the directory's caps,** but doesn't end the directory, which this one didn't launch. `Delete_Directory` still needs it to be empty.
- **`Add_Special_File` stays on `Writable_Directory`.** A stream can't make a cycle, so planting one in a home is ordinary writing.

## Consequences

- **Rights stay types.** The type of a directory cap says whether its entries can be written, with no exception for special files.
- **There are three directory interfaces where there were two.** Petal, the book and every command signature that names a directory have to choose between them. Most commands want `Directory` or `Writable_Directory`, as now.
- **A command can be given a log directory that it can append to but not change.**
- **No subprogram is pinned,** since nothing from before the move has to be restored (section 4).

## Alternatives considered

- **`Add_Directory` on `Writable_Directory`** (section 7). Every holder of a writable directory could then link, and make cycles that no walker of a `Tree` could detect.

- **Rights over the directory only** (ADR 0035, section 3, as merged). It is simpler, but a read-only directory can't then be read-only all the way down, and the type doesn't say which view a cap is.
- **A flag in a derived cap** (#116) for "read-only all the way down". The kernel would have to know about directories, and a command couldn't ask for the view in its signature. This is the kind of flag probing that Rose avoids: features are interfaces.
- **A membrane** (ADR 0001, layer 2) that narrows everything a directory returns. This gives case 2 from a writable directory without the server's help, and may still be useful, but it is another process for every view, and case 2 doesn't need one when the server can answer it directly.

## Open questions

- **Should `Fixed_Directory` add anything else**, such as truncating a file? Truncating changes the file, not the directory, so it belongs to the file's own interface.

## Follow-up work

- #938: the IDL, the directory server and isofs.
- #929: the system directory, against these interfaces.
