# 6. Signals

- **Status:** Proposed
- **Date:** 2026-10-26
- **Related:** [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0007](0007-networking.md) (networking)
- **Amended by:** #512, where a call can be made pausable, so that a signal reaches a process blocked in it without cancelling it (section 5)

## Context

A signal tells a process that something happened, without making the sender wait. Rose has no such thing, and several parts of the system need it.

### What exists today

- **IPC is synchronous.** A send with `Block` waits until the receiver is receiving on that endpoint. A send without `Block` fails with `Request_Would_Block` if it isn't.
- **The timer can lose expiries.** `Timer.Set_Timer` takes a cap, and the timer sends to it when the time is up, without `Block` (`Timer.Server`). If the process that set the timer isn't receiving at that moment, the expiry is lost.
- **The restore notice is a one-off.** The kernel sets a `Restore_Notice` flag on a restored process and sends it `Restorable.Restored` at its next receive, before anything else (ADR 0004).
- **Hardware interrupts are messages.** The kernel sends an interrupt to the driver's `Interrupt_Handler` endpoint, and holds one message for a driver that isn't receiving.
- **Process caps.** A process cap has endpoints for kill (`Kill_Process_Endpoint`) and for `Kernel_Process.Notify (Flags : Word_32)`. The kernel hands out the `Notify` cap but doesn't handle it. There is no way to suspend a process.

### Who needs signals

- **The network (ADR 0007).** A driver must tell `net` that frames have arrived without calling it, and TCP's retransmission timer depends on expiries never being lost.
- **The shell.** Ctrl-C should stop a job straight away, and a job should be stopped and resumed.
- **Restore.** The restore notice is a signal in all but name.

## Decision

### 1. A signal word in each process

- **Each process has 32 signal bits** in its kernel record, one bit for each signal, so the whole word fits in one invocation word. A bit is set when the signal is sent and cleared when it is delivered.
- **Signals are merged.** A bit that is set again before it is delivered stays set. A delivered bit means the signal was sent at least once since the last delivery, not how many times.
- **The word is saved in checkpoints** with the rest of the process record, as the `Restore_Notice` flag is now.

### 2. Named signals and the process's own signals

Bits 0 to 7 are named signals, defined by the system. Bits 8 to 31 belong to the process, which gives them their meaning.

| Signal | If not handled | Meaning |
|---|---|---|
| `Interrupt` | end the process | Ctrl-C: stop what you are doing. It is urgent (section 4). |
| `Terminate` | end the process | Please exit. A process that handles it finishes what it is doing, tidies up and exits. |
| `Stop` | suspend the process | Ctrl-Z. A process that handles it tidies up, then asks the kernel to suspend it (section 3). |
| `Restored` | ignore | The process has been restored (section 7). |

The other named bits are reserved. The order of the table is not a numbering: the bit for each named signal is defined once, in the Rose package that declares them, not in this ADR.

- **A process tells the kernel which named signals it handles,** through a kernel operation on its own process. When a named signal that the process doesn't handle is sent, the kernel takes the default action for it and doesn't set the bit.
- **The process's own bits have no default action.** They are always set and delivered.

A named signal is kept only where the sender mustn't wait, the notice mustn't be lost, or the kernel must act on a process that is busy or blocked. From POSIX, `SIGTERM` and `SIGTSTP` became `Terminate` and `Stop`, and `SIGKILL`, `SIGSTOP` and `SIGCONT` are operations on the process cap (section 3). The others need no named signal:
- **Faults** (`SIGSEGV`, `SIGFPE` and the like) go to `mem` or put the process in the kernel's `Faulted` state.
- **Timers** (`SIGALRM` and the like) are the process's own bits, set by the timer through a signal cap.
- **`SIGPIPE` and `SIGHUP`** are a `Closed` error from the next call on the stream or console that has gone away. A process that never uses it again doesn't need to know.
- **`SIGCHLD`.** The reply to the launch invocation (ADR 0002) tells the launcher that a command has ended. A shell that can't wait for the reply, for a background job, gives exec a signal cap for one of its own bits, and then asks which job ended. A signal couldn't say which, so it would have to ask anyway.
- **`SIGWINCH`.** A program that needs the terminal's size gives the terminal a signal cap for one of its own bits, through the terminal's interface, and asks for the new size when it is signalled. Only the programs that care are told.
- **`SIGTTIN` and `SIGTTOU`** exist because of a shared controlling terminal, which Rose doesn't have.

### 3. Kill, suspend and resume are operations on the process cap

Killing, suspending and resuming a process need no signal. The kernel carries them out when it is called, and calling the kernel never waits for the target process. They are endpoints on the process cap: `Kill_Process` exists now, and `Suspend` and `Resume` are new. Each is its own endpoint cap, so it can be given out and derived on its own (ADR 0001). The process can't see, handle or refuse them.

- **A suspended process is not scheduled,** whatever state it is in. If it was blocked in a call and the reply arrives, it stays suspended until it is resumed, and then sees the reply as usual.
- **Suspension is saved in checkpoints,** so a process suspended at a checkpoint is still suspended after a restore.
- **Other processes see no difference** between a suspended process and a slow one. A call to it waits.

### 4. `Interrupt`: acted on as soon as the process is scheduled

`Interrupt` must take effect at once, not when the process next happens to receive.

- **By default, it ends the process.** When the process is next scheduled, the kernel ends it, as `Kill_Process` does.
- **A process can handle it instead** (section 2). Then, when the process is next scheduled:
  - if it is blocked in an invocation, the invocation is cancelled and returns a new error, `Interrupted`;
  - otherwise, its next invocation returns `Interrupted` without being made.
- **The run-time system raises an exception** for `Interrupted`, so in Ada it is handled like any other exception, and a generated command main (ADR 0002) turns it into `Fail`.
- **A handling process that is computing** without invoking anything sees `Interrupted` only at its next invocation. The shell can still kill a job that doesn't stop. Handlers that break into running code would close this gap, and are an open question.

### 5. Ordinary signals: delivered at the next receive

The ordinary named signals that a process handles, and its own bits, are delivered when the process next receives, on any endpoint.

- **One message carries every pending bit,** in its first word, and clears them. It comes from the kernel and has no reply cap, as interrupts do.
- **It comes before any queued message or blocked sender,** as the restore notice does now (ADR 0004).
- **`Rose.Server` dispatches each bit** to the handler registered for it, and ignores bits with no handler. When a handler for a named signal is registered, `Rose.Server` tells the kernel that the process handles that signal. A process that registers none sees no change.
- **A process blocked in a call doesn't see them** until the call returns and it next receives, unless the call is pausable.

#### Pausable calls

Amended by #512. Some processes must hear of a signal while they wait for a long call, and then go on waiting for it. The console supervisor (#512) waits for the user's session to end, in its launch call to petal, and must handle `Restored` while it waits. A shell waits in the same way for a job in the foreground. Cancelling the call, as `Interrupt` does, doesn't do: the reply is lost, and making the call again would make the request again, a second launch.

- **A call can be made pausable,** with a new invocation flag. The run-time system sets it only where a handler may safely run, such as the supervisor's wait for petal. A call without the flag is not paused, so a process that doesn't ask sees no change.
- **A signal that the process handles pauses a pausable call that is waiting for its reply.** The call returns at once with a new status, `Signalled`, which isn't an error, and the pending bits in its first word, as at a receive. The bits are cleared.
- **The call stays open.** The server still holds its reply cap and answers it as usual. If the reply arrives before the process waits again, the kernel holds it for the process.
- **The process waits again with a new operation, `Wait_Reply`,** which returns the held reply at once, or blocks until it arrives. `Wait_Reply` is pausable too.
- **A call that hasn't reached the server yet is withdrawn instead.** The caller is taken off the server's waiting list, and the call returns `Signalled` marked as not sent. The run-time system makes it again after the handlers, which is safe, since the server never saw it.
- **The run-time system makes it look like one call.** It dispatches the bits to their handlers (`Rose.Server`), then calls `Wait_Reply`, and the caller sees only the reply.
- **A process has at most one paused call.** The calls that its handlers make aren't pausable, and signals that arrive during a handler wait until it returns to `Wait_Reply`. The kernel tells the paused call's reply from the replies to the handlers' calls by a call number in the reply cap.
- **A paused call is saved in checkpoints,** with any reply that is held for it. After a restore, a process in `Wait_Reply` goes on waiting for the server it called, as any blocked caller does (ADR 0004, section 4).

`Interrupt` is unchanged: it cancels the call (section 4).

### 6. Signal caps

- **A signal cap names a process and one bit.** It is a new cap type: the process's object id in the payload and the bit in the header, which fits the existing cap layout. Invoking it sets the bit, or, for a named signal that the process doesn't handle, takes the signal's default action. It never blocks and has no reply.
- **A process makes caps for its own bits,** through a kernel operation on its own process. It gives them to whoever should signal it: the timer, or a driver signalling `net`.
- **Caps for named signals come with the process cap.** Whoever holds a process's cap, usually the process that launched it, can make caps for its named signals, except `Restored`, which only the kernel sends. Exec gives petal those of each job it launches, with the job's kill, suspend and resume caps.
- **They can be derived and revoked** like any other cap (ADR 0001).

### 7. The timer and the restore notice use signals

- **`Set_Timer` is given a signal cap.** The timer invokes the cap it is given when the time is up. With a signal cap, an expiry can't be lost. The timer's interface doesn't change, but it should refuse caps that aren't signal caps, so the old, lossy path goes away.
- **`Restored` replaces the `Restore_Notice` flag.** The kernel sets the `Restored` bit on every process that restore resumes, and `Rose.Server` calls the process's `Restored` handler. A process that implements `Restorable` sees no change. ADR 0004's guarantee holds: the notice arrives before anything else the process receives.
- **A process blocked in a pausable call hears of a restore at once,** not when the call returns (section 5). This is what the console supervisor needs (#512).
- **`Kernel_Process.Notify` is removed.** Signal caps replace it.

## Consequences

**Benefits**
- A process can tell another that something happened without waiting for it, and the signal can't be lost.
- Timer expiries are never lost.
- The shell can stop, suspend and resume a job, and a job can't refuse to be killed or suspended.
- The shell can ask a job to exit or stop, and a job that doesn't handle the request gets the default action, as in POSIX.
- The restore notice stops being a special case.
- Sending a signal is a capability, so only the processes given a cap can signal a process, and the cap can be revoked.

**Costs**
- **Kernel work:** the signal word, a new cap type, cancelling a blocked invocation, and suspension in the scheduler.
- **Signals carry no data.** A process that uses one bit for several things, such as many timers, must work out which one happened.
- **24 bits for each process's own use.** A process with more sources than that must share bits between them.
- **Cancelling an invocation is new.** A server may already be working on the cancelled request, and its reply then has nowhere to go. The kernel must discard it safely.
- **Pausable calls are new kernel state:** an open call that outlives the system call that made it, a held reply, a call number in reply caps, and `Wait_Reply`, all saved in checkpoints.

## Alternatives considered

- **A pending bit for each endpoint.** A signal would be a message to an endpoint, kept if the receiver is busy. This needs state for each endpoint, and the receiver can't see all pending signals at once.
- **Counting signals.** A counter instead of a bit says how many times a signal was sent. It costs more space, and the senders that need a count can say so in a call.
- **Kill, suspend and resume as signals.** They would never set a bit, since the kernel carries them out at once, so a signal gives nothing that an operation on the process cap doesn't.
- **More named signals, following POSIX,** such as a child exiting, a hangup or a resized terminal. Each is better as a reply, an error, or one of the process's own bits (section 2).
- **Signals cancel every blocked call, as `Interrupt` does.** The caller would have to make the call again, which makes the request again. A launch would start a second process.
- **Asynchronous calls.** A call made without waiting, whose reply comes later as a message on one of the caller's endpoints. It is more general, but it changes how every reply is delivered. A pausable call keeps the call as it is, and adds only the pause.
- **A process that waits for each long call.** A helper makes the call and signals the caller when it returns. It needs no kernel change, but costs a process for each wait.
- **A contract for the program being waited for,** for example a petal that answers its launch at once and sends a message when it ends. It works only for programs that follow it.
- **Suspension that the process can see.** A process told that it is being suspended could refuse or delay it. Suspension is the shell's decision, not the job's.

## Open questions

- **Handlers that break into running code.** The scheduler could check a process's urgent signals each time it schedules the process, and act there: save the process's state and start it at a handler, as Unix signals do. A process computing without invoking anything would then see `Interrupt` straight away. But servers run without tasking, so a handler that runs in the middle of other code needs a run-time system that copes with it, and the kernel needs a way to return to the interrupted code. For now, cancelling the current invocation (section 4) covers the common case.
- **Hardware interrupts as signals.** An interrupt could set a bit instead of being sent as a message. That would remove the one-message slot that ADR 0004 works around, but a driver would lose the interrupt code that comes with the message.
- **Masking.** Should a process be able to hold off its own signals for a while, for example while it updates state that a handler reads?
- **More than one paused call.** A handler that itself makes a long call, and should be paused in it, would need paused calls to nest.
- **`Interrupt` and pausable calls.** `Interrupt` could pause a pausable call instead of cancelling it, and leave the handler to cancel it, through a new `Cancel_Call` operation.
- **Which caps petal gets.** Exec could give petal every named signal cap and process operation for a job, or only `Interrupt`, `Terminate` and kill, with `Stop`, suspend and resume for job control later.

## Follow-up work

- Kernel: the signal word, signal caps, and delivery of ordinary signals at receive, saved in checkpoints (new issue).
- Kernel: `Suspend` and `Resume` operations on the process cap, and suspension in the scheduler (new issue).
- Kernel: which named signals a process handles, and the default actions of the ones it doesn't (new issue).
- Kernel and run-time system: `Interrupt`, the `Interrupted` error, cancelling a blocked invocation, and the exception (new issue).
- `Rose.Server`: dispatch signal bits to handlers (new issue).
- Kernel and run-time system: pausable calls, `Signalled`, `Wait_Reply`, call numbers in reply caps, and saving a paused call in checkpoints (new issue).
- `Restored` as a signal, replacing the `Restore_Notice` flag (new issue).
- Timer: accept only signal caps (new issue).
- Exec: signal the launcher, through a signal cap it gives, when a process it launched ends, for background jobs (#142).
- Remove `Kernel_Process.Notify` (new issue).
