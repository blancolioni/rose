# 23. Ada.Command_Line in the userland run-time system

- **Status:** Proposed
- **Date:** 2026-11-02
- **Related:** #498 (this decision), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands, the launch invocation), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (the command is the transaction), [ADR 0017](0017-loading-ada-packages.md) (loading Ada packages), blancolioni/idle (the generated main)

## Context

GNAT's `Ada.Command_Line` reads `argc` and `argv` from the C run-time system. Rose has neither, and deliberately gives programs no process environment (the Environment page is going away). A command receives its arguments in a launch invocation, after it has started, and the shell binds them from the command's typed signature (ADR 0002).

A command with typed parameters has no use for `Argument (N)`. This package is for programs ported from other systems, and its design must not bend the typed path to serve them.

### What exists today

- **The generated main.** `Receive` waits for the shell's invocation, the main gives the standard streams to the run-time system, decodes each argument with `Rose.Command_Line.Launch`, calls the command, and ends with `Complete`. An exception becomes `Fail`.
- **At most one list argument.** A signature has at most one `Rose.Command_Line.String_List`, which takes the positional words that no other argument took.
- **Named arguments and options** (`-n`, `--no-new-line`) are bound by the shell from the signature, so they never reach the command as words.

## Decision

### 1. The arguments are the command's argument list

`Argument_Count` and `Argument (N)` report the words of the command's `String_List` argument, in order. A command that has none has no arguments, and `Argument_Count` is 0.

- **The shell has already done the parsing,** so the words are not a copy of what the user typed. `echo -n a b` gives `Argument_Count` 2 in a command whose `-n` is a named argument.
- **`Argument (N)` raises `Constraint_Error`** for N greater than `Argument_Count`, as in GNAT. (The run-time system's own `Rose.Command_Arguments.Argument` returns "", since it has no assertions.)
- **`Ada.Command_Line.Environment` and `Remove` don't exist.** There is no environment to report, and the words belong to the invocation.

### 2. They arrive after elaboration

The arguments arrive in the launch invocation, which the main receives after the program has been elaborated, so `Argument_Count` is 0 in a library unit's elaboration code and during `Receive`. A program that needs its arguments reads them from its main. This is the one place where Rose differs from GNAT that a program can notice.

### 3. The generated main fills them in

The run-time system holds the name and the words in `Rose.Command_Arguments`, a unit of its own beside `Rose.Standard_Streams`. `Ada.Command_Line` reads it, and so does nothing else. Before it calls the command, the generated main calls `Rose.Command_Line.Launch.Set_Command_Line` with the command's name and its list argument, if there is one:

```ada
Rose.Command_Line.Launch.Set_Command_Line
  ("Rose.Core.Echo",
   Rose.Command_Line.Launch.String_List_Argument (Call, 1, 1));
```

The words are copied into a static buffer of 4096 bytes, the size of a launch buffer, so no allocation is needed and the words outlive the invocation's own buffer.

### 4. The command name is the IDL name

`Command_Name` is the full name in the command's IDL declaration, such as `Rose.Core.Echo`. The generator knows it, so the launch invocation doesn't change. It is not the name that the user typed: the shell can register a command under another name, and the process has no way to know. ADR 0017 may give a package's name to a loaded command later; this would then be the one that `Command_Name` reports.

### 5. A status other than zero fails the command

`Set_Exit_Status (Code)` records the status. `Complete` replies with a failure, and the message `exit status N`, if the status is not zero, so the shell discards the command's outputs (ADR 0012), as it does when the command raises an exception. A status of zero is a success. `Success` and `Failure` are 0 and 1, as in GNAT, and a command can use any other value.

A failure reports the number in the message, and nothing else: the shell doesn't give a command's caller a numeric status.

## Consequences

- **`Ada.Command_Line` is a Rose unit,** `a-comlin`, in the run-time system's own source directory, not one of the GNAT units that are copied from the installed GCC.
- **The generator changes.** `idle` emits the `Set_Command_Line` call in every command's main, so the expected output of every command in its tests changes. A Rose build needs an `idle` that does.
- **A ported program can't see its options.** The shell binds `-x` as a named argument by the signature, so a program that parses its own options gets only the positional words, and the shell may refuse an option that the signature doesn't declare. A command aspect that asks the shell to pass every word through unparsed would fix this, and is not part of this decision.
