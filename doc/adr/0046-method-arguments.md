# 46. Method arguments and named subsets

- **Status:** Proposed
- **Date:** 2026-11-26
- **Related:** #422 (this decision), [ADR 0001](0001-capability-revocation.md) (derive and membranes), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands, section 5 and the open question on attenuation), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (the type is the right), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), [ADR 0027](0027-new-files-in-a-transaction.md) (new files in a transaction), [ADR 0034](0034-interface-packages.md) (interface packages), [ADR 0036](0036-directory-views.md) (directory views)
- **Amends:** [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md), section 2, so that the type of an argument can be a method or a named subset of an interface, as well as an interface

## Context

ADR 0012 has the type of an argument give the right. The smallest right a command can ask for is a whole interface, so a command that needs one operation gets all of them:

- `remove` needs `Delete_File`. Its `in Files.Writable_Directory` argument also lets it create files and directories, rename entries, delete directories and add special files.
- `rmdir`, `rename` and `mkdir` are the same, each with a different single operation.

Giving each operation an interface of its own doesn't scale. Every operation has the same case for it, and an interface with one operation is just a method under another name.

ADR 0002 left attenuation as an open question: should narrower grants come from IDL types, from derive, or from membrane filters? ADR 0012 answered it for directories by splitting the interface (`Directory`, `Fixed_Directory`, `Writable_Directory`), which is as far as splitting goes.

The run-time cost of a narrower grant is already paid. A server has one endpoint for each operation, and a generated client holds a cap for each one (`Open_Cap_Set`, `Get_Delete_File_Cap`). A cap for one method is a cap the shell already has.

## Decision

### 1. An argument's type can be a method

```ada
command Rose.Core.Remove : Streams.Standard_Error
is
   Name : String
     with Help => "the file to remove";
   Directory : in Files.Writable_Directory.Delete_File
     with Help => "the directory that holds it";
end Rose.Core.Remove;
```

- **The type is `Interface.Method`.** The interface prefix is the view the method is taken from (ADR 0036), and the method is one that the interface declares or inherits.
- **The command can call that method and nothing else.** Idle generates a client type for the argument that has only that operation, so calling anything else is a compile error.
- **It is passed as one cap,** the method's endpoint cap. The generated main opens the client from that cap directly, with no call to the server.
- **There is no way back to the interface.** A method's endpoint doesn't answer `Get_Interface_Cap` or any other method, so a command given `Delete_File` can't turn it into a `Writable_Directory`.
- **A method that returns an interface returns all of it.** A command given `Files.Writable_Directory.Create_File` gets a whole `Writable_File` for each file it creates. The grant limits what the command can do to the object it was given, not to the objects it makes.

### 2. A named subset is a type of its own

A command that needs several operations on one object declares a subset in its interface package, and takes that:

```ada
subtype Files.Deleter is Files.Writable_Directory
  with Only => (Delete_File, Delete_Directory);
```

```ada
   Directory : in Files.Deleter
     with Help => "the directory to delete from";
```

- **The methods stay on one object.** Two method arguments could be bound to different directories. A subset can't be.
- **It is nominal,** like the rest of idle. Two subsets with the same methods are still different types.
- **The name is shown to the user.** `--help` and petal's error messages say `Files.Deleter`, not a list of methods.
- **It reads as an Ada subtype:** the same object, with fewer things allowed on it.
- **A single method is the one-element case.** `Files.Writable_Directory.Delete_File` means the same as an unnamed subset with `Only => (Delete_File)`, and needs no declaration.
- **It is passed as one cap for each method,** in the order of the `Only` list.

### 3. No unnamed subsets in a signature

`in Files.Writable_Directory with Only => (Delete_File, Delete_Directory)` written in a command's signature is not allowed. It saves one declaration and loses the name in section 2. A single method (section 1) is the exception, because its name is already the method's name.

### 4. Petal binds a method or subset as it binds the interface

- **Petal looks up the word as now,** in its directory table, with the same default.
- **It narrows to the prefix's view** (ADR 0036), then sends the cap for each method instead of the interface cap.
- **The directory server doesn't change.** It already answers each method on its own endpoint.
- **A missing right is reported as it is now.** If petal holds only a `Fixed_Directory` for the word, it can't grant `Files.Writable_Directory.Delete_File`, and it says so with the argument's type.

### 5. Transactions follow the object

A method cap is a cap on whatever object it was taken from.

- **Under ADR 0026, a version belongs to the object,** so a method of a version is a method cap taken from the version.
- **Directories have no versions yet** (#634, ADR 0027). A method of a `Writable_Directory` is live, as the whole directory is today.
- **A method or subset of a `Versioned` interface is `Versioned`,** so the command commits it as it would commit the interface.

### 6. Attenuation, ADR 0002's open question

- **A grant that idle can check when it compiles a command is a type:** an interface, a method or a named subset. It is in the command's signature, and the user sees it in `--help` before the command runs.
- **A grant decided when it is made,** such as the user narrowing an argument at the prompt or a broker refusing part of a role, is left to derive and membranes (ADR 0001). This ADR doesn't decide how those work.

### 7. What waits

- **Interface lists (ADR 0012, section 6).** A method or subset can't be in an `or` list yet. A membership test needs a tagged client of the interface, which a method client isn't.
- **Currying.** Petal could bind a method's parameters as well, so that `Delete_File (Name)` becomes a cap with no parameters that deletes one file. The command would never see other names. This waits for ADR 0027 to settle how a new name is bound.
- **Roles.** A role's type stays an interface for now.

## Consequences

**Benefits**
- A command asks for exactly the operations it uses, and its signature shows them.
- `remove`, `rmdir`, `rename` and `mkdir` each get one operation of a directory, not all of them.
- No new interfaces are needed to make a grant narrower, and the directory server doesn't change.
- A command can't regain a method it wasn't given (section 1).

**Costs**
- **Idle:**
  - parse a method type `Interface.Method`, and `subtype ... with Only`;
  - record the method or subset in the signature and the metadata, so that petal can tell it from an interface;
  - generate a narrowed client type, opened from the caps the command receives;
  - refuse a method or subset in an `or` list.
- **Petal:** narrow to the view, then send the method caps instead of the interface cap.
- **Launch caps:** a subset takes one cap for each method, so a wide subset uses more of the launch call's caps than the interface would.
- **Commands:** `remove`, `rmdir`, `rename` and `mkdir` change their argument types.
- **The Rose Book** needs a section on method arguments.

## Open questions

- **A subset as one cap.** A subset could be passed as one cap if the server answered a cap set for it, but that changes every server. Is the cost in launch caps (section 2) worth avoiding?
- **Where subsets are declared.** In the interface package (ADR 0034), so that everyone can reuse them, or also in a command's own package, for a subset only that command needs?

## Follow-up work

- Idle: method types and named subsets (blancolioni/idle, issue to be opened).
- Petal: bind method and subset arguments.
- `remove` takes `Files.Writable_Directory.Delete_File`, `rmdir` takes `Delete_Directory`, `rename` takes `Rename`, and `mkdir` takes `Create_Directory`.
- Rose Book: method arguments.
- ADR 0012, section 2: a pointer to this ADR, and its open question on narrower grants marked as answered.
