# 41. Petal sessions: kept jobs, a terminal for each, attached from any console

- **Status:** Proposed
- **Date:** 2026-11-22
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands, sections 8 and 9), [ADR 0005](0005-persistent-console.md) (the persistent console), [ADR 0006](0006-signals.md) (signals), [ADR 0014](0014-banks.md) (banks), [ADR 0015](0015-process-ownership.md) (process ownership, sections 3 and 4), [ADR 0018](0018-credentials-after-restore.md) (credentials after a restore), [ADR 0022](0022-display-server.md) (the display server), [ADR 0040](0040-package-state.md) (package state), #142 (background job handles), #749 (a terminal server for each job), #119 (petal runs commands inside a membrane), #507 and #508 (lock and suspension after a restore), #517 (a bank for each command), #655 (switching between the console and the display server), #1037 (`scheme`), #1039 (Z-code interpreter)

## Context

Rose's processes persist, but the way they are used doesn't take advantage of it. A user who is in the middle of a Z-code game, a Scheme session or an edit and wants to do something else has to end the program, perhaps saving first, and start it again afterwards. A REPL whose state should last for weeks is started and stopped like any other command.

Unix shells answer part of this with job control (`^Z`, `fg`, `bg`), and terminal multiplexers such as tmux answer the rest: a program keeps running when nobody is looking at it, and can be looked at again from another terminal. On Rose both should come from the same mechanism, and neither should need the program's cooperation.

### What exists today

- **A session is a bank.** Login asks the user server for `New_Session`, a sub-bank of the user's bank, launches petal against it, and destroys it when the session ends, which ends everything the session launched (ADR 0015, #450).
- **Petal runs one command at a time, in the foreground.** It launches against its own bank, and gives each command a bank of its own for `Storage_Service` (#517).
- **There is one terminal per screen.** The console serves `Terminal` itself, with one set of modes (line editing, echo, attributes), and petal resets them when a command it gave the console to ends (#734). #749 proposes a terminal server with one terminal for each job, and doesn't decide it.
- **Background jobs are planned, not built.** ADR 0002, section 8, and #142: `&` returns a handle, and a job that outlives the session needs a bank outside it (ADR 0015, section 3).
- **A restore locks the console and suspends a login's caps** until the user authenticates again (ADR 0018, #507, #508).

## Decision

### 1. Leaving a job detaches it, and doesn't stop it

- **Detaching takes the screen and keyboard away from a job** and gives them back to petal. The job's process carries on as it was. A REPL or a game waiting for a key uses nothing while it waits.
- **Detaching is not suspending.** ADR 0006's `Suspend` stops a process from being scheduled. That stays a separate operation, for a job that is busy computing and should stop for now.
- **The program doesn't know.** Nothing is delivered to it, and it needs no code for this.

### 2. Each job has its own terminal

This decides #749: a terminal server serves `Terminal`, one terminal for each job, and the console serves only cells and keys.

- **A job's terminal holds its own cells,** cursor, attributes, line-editing and echo modes, and line buffer.
- **Attached, it is drawn on a console's screen,** and takes its keys from that console.
- **Detached, it carries on.** What the job writes goes into its cells, so attaching again shows the screen as the job left it. A `Read` or `Read_Key` waits until it is attached and a key is typed.
- **The size is the screen's it is attached to.** Attaching to a screen of another size changes it, and the job is told (as #749 requires of `Terminal` before it freezes).
- **Ending the job ends its terminal,** and with it the modes, so petal no longer resets the console after a command (#734's `Reset_Console` goes).

### 3. A key that only the console sees

- **The console reserves one key combination,** which it never passes to a terminal. Pressing it detaches the attached job and gives the screen back to petal.
- **Every other key goes to the attached job,** including the one that interrupts it (ADR 0006, section 4).

### 4. Jobs are the session's until they are kept

| How a job starts or is left | Whose it is | Ends when |
|---|---|---|
| A command, in the foreground | the session's | it exits, is ended, or the session ends |
| `command &` (#142) | the session's, never attached | the same |
| Detached with the console's key | **kept**: the user's | it exits or is ended |
| `keep job` on a session job | **kept**: the user's | it exits or is ended |

- **Keeping moves the job's bank** from under the session's bank to under a bank that the user keeps for jobs. ADR 0015 makes outliving the session a matter of whose bank a process depends on; this does the same, but after launch, not before.
- **The store gains a way to move a bank:** `Bank.Adopt (Child)` makes `Child` a sub-bank of the bank it is called on, if the caller holds both and the new parent's limits allow it. Usage moves with it.
- **Keeping is a deliberate act,** as ADR 0002, section 8, requires: the detach key or `keep`. Nothing is kept by accident, and a foreground job still ends with the session.

### 5. The user's jobs table

- **The user server keeps a table of the user's kept jobs,** behind an interface of its own, `Users.Jobs`, reached through the user's `User` cap. Every session of the user, on any console, sees the same table.
- **Each job has a name:** the command's, with a number if the name is taken (`kilo`, `kilo-2`), which the user can change. Handles (#142) stay for scripts; names are for people.
- **The table holds what keeping a job needs:** the job's `Process_Control`, its bank, its terminal and its membrane (section 7).

Illustrative only:

```ada
interface Users.Jobs is
   function Job_Count return Natural;
   function Job_Name (Index : Positive) return String;
   procedure Rename (Index : Positive; Name : String);
   function Attach
     (Index : Positive;
      Screen : interface Screens.Cells)
      return interface Screens.Attachment;
   procedure End_Job (Index : Positive);
end Users.Jobs;
```

### 6. Attaching, from any console

- **`jobs`** lists the kept jobs, and the session's own, with where each is attached.
- **`attach name`** gives the job's terminal this console's screen, and petal waits until the job is detached again or exits. Then petal redraws its own screen.
- **One screen at a time.** Attaching a job that is attached somewhere else detaches it there, and that console's petal gets its screen back with a line saying where the job went.
- **`end name`** ends a job: it sends ADR 0006's `Terminate`, and destroys the job's bank if the job doesn't exit (ADR 0015, section 5).
- **Not only consoles.** A terminal can be attached to anything that serves cells and keys: a window under the display server (ADR 0022, #655), or a test double. A terminal over the network would be the same.

### 7. A kept job keeps its caps

- **Keeping a job keeps its caps.** A job is kept so that it can carry on, and it can't carry on without them.
- **Each job's membrane (ADR 0002, section 9) belongs to the job,** not to the session. The jobs table holds it once the job is kept, so ending the session doesn't revoke a kept job's caps. Ending the job does.
- **Logging out doesn't touch them.** A kept job's caps came through the login that started it (ADR 0018, section 3), but ending a session only ends the session's bank and the membranes of the session's own jobs. Nothing rescinds a login's subtree when the login ends.
- **A privileged grant stays with the job too.** The broker revokes it when the job ends (ADR 0021, section 2), not when the session does.
- **A restore suspends them, as ADR 0018 says,** and the user's next login resumes them. A kept job waits in its next call until then, as any job of a suspended login does.

### 8. Restores

- **Kept jobs are persistent processes,** so a restore brings them back with their terminals, their cells and their attachments.
- **The console locks** (ADR 0018), and shows the saved screen only once the user has authenticated, whichever job it belongs to.

## Example

```
petal> scheme
> (define (fib n) ...)
> ^\                          the console's detach key: scheme is kept
petal> zcode games/curses.z5
...
> ^\
petal> jobs
  scheme   kept   detached
  zcode    kept   detached
petal> attach scheme
> (fib 20)
6765
```

On another console, the same user's `attach scheme` takes it there, and the first console's petal says so.

## Consequences

- **Long-lived programs are just kept jobs.** `scheme` (#1037) is started once and attached to when wanted. Nothing is saved to switch away from a game or an editor.
- **No program needs code for any of this.** Detaching, keeping and attaching happen in the console, the terminal server, petal and the user server.
- **#749 is decided** in favour of a terminal server, with what that costs: one more IPC hop for each key and write, and one more process to start and restore.
- **ADR 0015, section 4, changes:** a job can be kept after it is launched, by moving its bank. The store needs `Adopt`, and the rule that a bank's parent is fixed goes.
- **The user server grows** a jobs table and the caps it holds for each job.
- **Kept jobs use storage** until they are ended, charged to the user's bank. A user can accumulate them; `jobs` makes them visible.

## Alternatives considered

- **Launching every job against the user's bank from the start,** so that there is nothing to move. It needs no `Adopt`, but every job would outlive the session unless it was ended, which ADR 0002, section 8, rules out.
- **Suspending instead of detaching,** as `^Z` does on Unix. A suspended REPL can't finish a computation in the background, and a suspended process still holds the console's modes unless something resets them.
- **A multiplexer program,** like tmux, that petal runs under. It would hold the jobs' terminals itself, and die with its own session. Putting the table in the user server and the terminals in a terminal server makes kept jobs independent of any one session.
- **Petal keeps the jobs table.** A kept job would then be found only from the session that kept it, and lost when that session ended.

## Open questions

- **The detach key.** It must be one that programs rarely need. `Ctrl-\` and a key the keyboard doesn't otherwise send are candidates.
- **Watching without taking.** Should a second console be able to show a job's screen read-only, while it stays attached elsewhere?
- **What petal shows at login** when the user has kept jobs: a line saying how many, or the list.
- **Limits on kept jobs:** whether the jobs bank should have a limit of its own.

## Follow-up work

- #749: the terminal server, and consoles that serve cells and keys.
- The store: `Bank.Adopt`.
- The user server: `Users.Jobs` and its table.
- The console: the detach key.
- Petal: `jobs`, `attach`, `keep`, `end`, and `&` (#142), which runs a job of the session with a terminal of its own.
- ADR 0015, section 4, and ADR 0002, section 8: point here.
