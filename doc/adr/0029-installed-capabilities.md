# 29. Installed capabilities: what a program brings with it

- **Status:** Proposed
- **Date:** 2026-11-06
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands, which this amends), [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0006](0006-signals.md) (signals), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (argument modes), [ADR 0014](0014-banks.md) (banks), [ADR 0017](0017-loading-ada-packages.md) (loading Ada packages), [ADR 0020](0020-clock.md) (the clock), [ADR 0021](0021-privilege.md) (privilege), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions)

## Context

ADR 0002 makes a command's signature its sandbox manifest: the command gets exactly the caps its arguments resolve to, and the caller supplies every one of them. That works for `copy` and `list`, whose caps are all about the call. It doesn't fit a program that needs data of its own.

- **A Scheme interpreter** such as Raven (blancolioni/raven) reads its standard library, `(scheme base)` and the rest, from a directory of `.scm` files. On Unix the directory is a path compiled into the program (`Raven.Standard_Library_Path`), and anyone can open it.
- **A compiler** needs far more. GCC's driver needs `cc1`, `collect2`, its specs, `libgcc` and its own headers, all from its own installation. It also needs libc's headers and `crt*.o`, which another package installs, and `as` and `ld`, which binutils installs.

Under ADR 0002, every user who runs one of these programs has to hold every one of these caps and pass them in each time, or have petal bind them from the user's environment. Either way, each user's environment has to know the configuration of every program they might run. Those caps belong to the program, not to the user. On Unix they are ambient (`/usr/share`, `/usr/lib/gcc`), and Rose has no ambient authority to fall back on.

### What exists today

- **`Exec.Install` takes `Install_Caps`.** Exec keeps them with the installed program, and `Launch` sends them to the new process ahead of the caller's caps, by position. Init uses this for system servers.
- **`Launch_Command` sends none.** A command has only the caps it is given (ADR 0002), so exec leaves the install caps out.
- **Roles bind system services.** A command that declares `Clock_Service` gets the session's `Clock` from petal (ADR 0020), and a test can bind a fixed clock instead.
- **Privileged roles go through the broker** (ADR 0021). Nobody holds `Clock_Admin` by default.

## Decision

### 1. Installation caps and invocation caps

A program's caps are of two kinds:

- **Installation caps** belong to the installed program: its library, its data, the other programs it runs. They are the same for every user and every run, and the program only reads them.
- **Invocation caps** belong to the run: the files it works on, its streams, the user's own directories. The caller supplies them, as ADR 0002 says.

The installer supplies installation caps, once, when it installs the program. Whoever holds the program's launch cap can run it with them, and never holds them.

### 2. A command declares its installed arguments

An installation cap is an argument of the command, with the `Installed` aspect, so the signature still lists everything the command can reach:

```ada
command Raven.Repl : Standard_IO
is
   Standard_Library : in Directory
     with Installed => "library",
          Help      => "the R7RS libraries, such as (scheme base)";
end Raven.Repl;
```

- **`Installed` names what to bind** (section 4). The installer resolves the name when it installs the command.
- **The argument is visible.** `--help` lists it, with what it is bound to, such as `(installed: raven 0.1, library)`. Nothing the command can reach is hidden from the user.
- **A caller can override it** by passing a cap of its own, as with any default: `raven --standard-library=~/scheme-dev` while working on the library. That gives the command nothing that the caller doesn't already hold.
- **Petal doesn't bind it.** If the caller doesn't name the argument, petal sends nothing for it, and the command uses its installed cap.

### 3. Exec keeps the caps, and the launch cap carries them

- **The installer passes the caps to `Exec.Install`**, in the order that the command's metadata lists its installed arguments.
- **`Launch_Command` sends them to the new process**, as `Launch` does for servers. They don't go through petal.
- **The generated main uses the installed cap** for an installed argument that the caller didn't pass, and deletes it if the caller passed one.
- **Holding the launch cap is the right to run the program with its installation caps.** The launch cap doesn't give them out. A derived launch cap (ADR 0001) carries them too, so a user can pass the program on.
- **A second installation** of the same program, with another library, is another `Exec.Install` and another launch cap. There is no search path to set.

### 4. What can be installed

Only caps that give no right to change anything:

| Type | Example |
|---|---|
| a lookup-only `Directory` | Raven's library, GCC's headers |
| `File` | GCC's specs |
| `Launch` | `cc1`, `as`, `ld` |

- **The installer refuses any other type**, and idle refuses `Installed` on an argument of another type. The list grows only by amending this ADR.
- **`Launch` is allowed** because running another program gives only that program's installation caps, which are read-only by this rule, and whatever the caller passes it.
- **Per-user state is never installed.** A history file, a cache or a user's configuration is writable and belongs to the user. It is an ordinary argument, which petal can bind from the user's named directories (ADR 0002, section 5).
- **System services stay roles.** The clock, the timer and storage are the session's to give, and a test or a membrane can substitute them. Installing one would take that choice away.
- **Privileged roles are never installed.** An installed `Clock_Admin` would give the right to set the time to everyone who holds `date`'s launch cap. ADR 0021's broker stays the only way to get one.

### 5. Names: a package's own data, and other packages' exports

- **A package's install image has a data tree** beside its executables. `Installed => "library"` names a path in the command's own package's tree. The installer binds a lookup-only `Directory` or a `File` for that path.
- **A package exports what other packages may use.** Its IDL `package` declaration gains an `Exports` aspect, listing paths in its data tree and its commands. Nothing else in the tree is visible to other packages.
- **Another package's export is named with its package:** `Installed => "Libc.include"` for a directory, `Installed => "Binutils.As"` for a launch cap.
- **The installer resolves names when it installs.** A package that isn't installed, or an export it doesn't declare, fails the installation and names what is missing, before anything runs. This is ADR 0017's rule for bodies, applied to data.
- **The registry keeps each package's exports,** beside its commands (ADR 0002, section 3), so a later installation can resolve against them.

### 6. Versions

- **Installation caps are fixed by the installation.** A command keeps what it was installed with, as a program keeps the bodies it was installed against (ADR 0017).
- **Reinstalling a package** gives new launch caps, and the registry entries point to them. Running processes keep the caps they started with.
- **A package that depends on one that changes** keeps the old caps until it is installed again. Whether installing a package should reinstall its dependents is an open question.

### 7. Jobs, membranes and sub-launches

- **Installation caps don't cross the job's membrane.** They come from exec, not from petal. The membrane exists to take back what the user granted (ADR 0001), and these were never the user's. When the job ends, its processes end, and their caps with them.
- **Sub-launches need nothing new.** GCC's driver launches `cc1` through its installed `Launch` cap, passing the source file and include directories it holds. `cc1`'s own installation caps come from its own installation.

## Example: Raven at the REPL

A Raven REPL for Rose would declare:

```ada
command Raven.Repl : Standard_IO, Clock_Service
  with Summary => "Read, evaluate and print Scheme expressions."
is
   Standard_Library : in Directory
     with Installed => "library",
          Help      => "the R7RS libraries, such as (scheme base)";
   Libraries : in Directory
     with Help => "where import, load and include look first";
   Files : in Writable_Directory
     with Help => "where open-input-file and open-output-file find files";
   Arguments : Rose.Command_Line.String_List;
end Raven.Repl;
```

`Raven.Target` would look a library up through the directory cap (`Find ("scheme")`, then `"base.scm"`), and not build a path.

| Need | Where it comes from | Notes |
|---|---|---|
| Standard input, output and error | `Standard_IO` | Line editing wants the console's `Terminal`, not only a `Reader`. |
| The standard library | installed | This ADR. |
| The user's libraries, `load`, `include` | `Libraries` | Searched before the standard library. |
| Files opened by name | `Files` | Names are only known at run time, so this is a directory grant. A powerbox would be narrower (open questions). |
| `current-jiffy`, timing the collector | `Clock_Service`, `Monotonic` | Today's `Ada.Calendar` use in `Raven.Engine.Memory` should be monotonic. |
| `current-second` | `Clock_Service`, `Realtime` | |
| Heap | the process's bank (ADR 0014) | The user's bank pays for it. |
| Ctrl-C | `Interrupt` (ADR 0006) | See below. |
| `command-line`, `exit`, `emergency-exit` | `Arguments`, and the command's reply | |
| `get-environment-variable(s)` | nothing | There is no environment page. Raven returns `#f` and the empty list. |
| A history file | an ordinary argument | Per-user, so never installed. |

- **Interrupting an evaluation.** A REPL should stop a runaway evaluation and return to its prompt. ADR 0006, section 4 delivers `Interrupted` at the process's next invocation, and `(let loop () (loop))` makes none. Until handlers can break into running code, the evaluator has to check for a pending signal every so many instructions.
- **Committing.** A REPL never ends in the way that `copy` does. Under ADR 0026 it commits each file it writes, which fits a Scheme port: `close-port` on an output file commits it.
- **Persistence.** The REPL's heap survives checkpoints, so Raven needs no saved images.

## Consequences

**Benefits**

- A user runs Raven or GCC with its launch cap alone. Their environment doesn't need to know any program's configuration.
- The signature still lists everything a command can reach, installed or not.
- A program's data is fixed when it is installed, and a missing dependency fails the installation, not a run.
- Overriding an installed argument is an ordinary argument, so testing a new library needs no second installation.
- Exec's install caps, which init already uses, now serve commands too.

**Costs**

- **ADR 0002 changes.** "The program receives exactly the capabilities its arguments resolve to" still holds, but an installed argument resolves to the installer's cap, not the caller's.
- **Idle:** the `Installed` aspect, its type check, its place in the metadata, and a generated main that chooses between the installed cap and the caller's.
- **Exec:** `Launch_Command` sends the install caps.
- **Petal:** it leaves installed arguments alone, and `--help` shows what they are bound to.
- **The installer and the registry:** package data trees, `Exports`, name resolution, and the package table.
- **A program can leak its installation caps,** by passing them to a server. They are read-only package data, and the program is one the user chose to install.

## Alternatives considered

- **The user holds them.** Every user's environment would carry the configuration of every program they might run, and a program's dependencies would change when they move. That is the problem this ADR solves.
- **The registry entry holds them, and petal merges them in.** This works, but petal then holds every program's caps, and a program launched by anything but petal misses them.
- **A wrapper server around the launch cap,** holding the real launch cap and the extra caps. It needs no change to exec, but it is a process for each installed program, and exec already keeps install caps.
- **Building the data into the executable.** It works for a small library, but not for GCC, and every change to the library would mean relinking.

## Open questions

- **Reinstalling dependents.** When libc's headers change, should GCC be installed again by itself, or should the registry rebind its caps?
- **Forbidding an override.** Can a command insist on its installed cap? Nothing here needs it.
- **Optional cap arguments.** Raven's `Libraries` and `Files` should be optional, and default to none, or to petal's current directory (#342). Whether a cap argument can be optional isn't settled.
- **A powerbox.** A program that finds out at run time which file it wants could ask the console to let the user choose one, and get only that file, instead of a directory grant.
- **Upgrading installed `Launch` caps.** GCC holds binutils' `as` from when it was installed. A newer `as` is reached only through a reinstall.

## Follow-up work

- Idle: the `Installed` aspect, the type check, the metadata, and the generated main.
- Exec: send install caps from `Launch_Command`.
- Petal: leave installed arguments unbound, and show them in `--help`.
- The installer and the registry: data trees, `Exports`, name resolution and the package table.
- ADR 0002: point section 1's "the signature is the sandbox manifest" here.
