# 18. Credentials after a restore

- **Status:** Proposed
- **Date:** 2026-10-30
- **Related:** [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0003](0003-directories-files-and-users.md) (users), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0005](0005-persistent-console.md) (persistent console), [ADR 0015](0015-process-ownership.md) (sessions), #453 (login), #455 (auth), #457 (login command), #507 (console lock), #508 (suspending a login's caps)

## Context

Restore brings back every persistent process with the caps it held. That is the point of it, but a login is state that restore shouldn't bring back as it was.

Today, `login` asks for a name and password, calls `Auth.Login`, and launches petal with the `User` interface it got. `login` then blocks in its call to petal until petal logs out. If Rose is restored while petal is running:

- **The session is back, and nobody has authenticated.** Whoever is at the keyboard is that user. The restore may follow a power cycle, or hours after the user left, and it may be a different person.
- **The console shows what the user was doing.** The screen is lost at a restore, but vga-console keeps a copy in its persistent memory and redraws it (ADR 0005).
- **Nothing is told to ask again.** `Restorable` (ADR 0004) is delivered when a process next receives. `login` is blocked in a call, which ADR 0004 says a restore doesn't make safe, so it hears about the restore only when petal exits.
- **It isn't only the console.** Any process that holds the user's caps, such as a background job, still has them. ADR 0003 says it plainly: holding a user's cap is being that user.

This is a case of something more general. A restore stretches time and can change who is present, so authority that was granted on the strength of an authentication needs to be confirmed again.

## Decision

### 1. A restore ends the authentication, not the session

After a restore, nobody is authenticated until they authenticate again. The session itself is kept: its processes, their memory and their caps are as the checkpoint left them. A user who unlocks the session finds their jobs still running.

### 2. The console locks, before it handles any input

vga-console is `Restorable`, and the kernel delivers `Restored` before anything else it receives (ADR 0004). On `Restored` it locks:

- **It shows a lock screen.** It doesn't redraw the screen it saved.
- **It doesn't deliver input to the session.** The console is the line discipline (ADR 0005, #454), so keystrokes reach the lock prompt and not petal.
- **The unlock check is `Auth.Login` for the session's user,** so only their password unlocks it.
- **On success it unlocks:** it redraws its saved screen and delivers input again.
- **The lock is part of the console's saved state.** A checkpoint taken while locked restores locked, and a restore always locks, whatever the checkpoint held.

The console doesn't check passwords itself. It is a driver, and `auth` is where credentials are. The lock prompt belongs to something that holds an `Auth` client for the session's user. It can't be `login`, which is blocked in its call to petal, so `login` registers a prompt with the console when it starts a session (#507).

### 3. A login's caps are held so that `auth` can suspend them

The console lock stops the person at the keyboard. Authority that a login gave to other processes is stopped in a way that doesn't depend on them cooperating, because an untrusted process can't be relied on to give up its caps.

- **A derived cap per login.** `Auth.Login` returns a `User` cap derived (ADR 0001, layer 1) from the user server's, and what a session holds comes from that. Each login is one subtree of the derivation tree.
- **Suspension.** ADR 0001's derivation records gain a flag. A cap that is suspended, and every cap derived from it, fails with an error of its own. It isn't `Invalid_Capability`, so a holder can tell that it must authenticate again, and it isn't permanent, as rescind is.
- **`auth` is `Restorable`.** On `Restored` it suspends every login's subtree, before it handles any other request.
- **Resume.** When the user has authenticated again, as in 2, `auth` resumes the subtree. Holders keep their caps and carry on, without having to be given new ones.
- **The same mechanism covers more.** An idle timeout, suspending one user, and the caps of network connections later are suspensions that `auth` makes for another reason (#508).

### 4. Console and caps are separate

The lock stops input and hides the screen. Suspension stops the authority that processes hold. Either is useful alone: a console with no suspension still locks out the person at the keyboard, and suspension still stops a background job when the machine isn't at a console. The decision needs both.

## Consequences

- **The session survives a restore.** A user who unlocks it finds their jobs, as a screen lock does. A restore stays an ordinary event and doesn't cost anyone their work.
- **Jobs keep running while the console is locked,** until they use a suspended cap. A job that holds only caps that aren't from the login, such as a computation, is not stopped.
- **`login` gets more complicated.** It has to give the console a way to prompt, because it is blocked in a call.
- **The kernel gains suspension.** A flag on derivation records, checked on every invocation of a derived cap and saved in the checkpoint. It depends on the derive operation (#115).
- **Caps outside the chain are not suspended.** The console, `Find_Command` and similar caps don't come from the login, which is right. Caps that a process obtains by calling a suspended cap are in the chain only if they are derived from it.
- **Memory and hardware caps can't be suspended this way** (ADR 0001, layers 2 and 3). A process with a mapped page keeps it.
- **A disk that is stolen isn't protected.** A checkpoint holds the screen, memory and caps of the session, unencrypted. This decision is about a restore, not about data at rest.

## Alternatives considered

- **End the session at a restore.** Destroying the session's bank (ADR 0014 and ADR 0015) would end petal and all its jobs, and show the login prompt. It is the simplest and the strictest, and it is unacceptable: a restore happens after every reboot, and every restore would end every user's work.
- **Each process drops its credentials on `Restored`.** It would need every process to cooperate. A process that doesn't is the one the user needs protection from.
- **Rescind the login's caps, and issue new ones after authentication.** It works with what ADR 0001 has, but the holders have to be given the new caps, and petal holds the old `User` as an argument it can't replace. Suspension keeps the caps.
- **Lock only petal, not the console.** Petal is the user's own process and can't be relied on to hide what the console redraws or to refuse input.
- **Decide by how long the machine was down.** A restore doesn't know how much time has passed, or whether the person at the keyboard changed.

## Open questions

- **Who may unlock.** Only the session's user, or also an administrator, and how an administrator would be known.
- **Failed attempts.** `login` waits after a wrong password. The lock needs the same, and perhaps a limit.
- **Output while locked.** What background jobs write to the console while it is locked: hold it in the console's buffer, or drop it.
- **Suspension in the kernel.** Whether it is a new kernel operation, or can be done with rescind and reissue at the cost of replacing caps, as in the alternatives.
- **Caps already passed on.** A server that was sent a cap before it was suspended keeps what it did with it. A suspension doesn't reach into a server's own state.
- **More than one console.** Each would lock for its own session.

## Follow-up work

- Console: lock on restore, and unlock by authenticating again (#507).
- Credentials after a restore: suspend a login's caps until the user authenticates again (#508).
