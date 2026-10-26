# 6. Signals

- **Status:** Proposed
- **Date:** 2026-10-26
- **Related:** [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0007](0007-networking.md) (networking)

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

- **Each process has 64 signal bits** in its kernel record, one bit for each signal. A bit is set when the signal is sent and cleared when it is delivered.
- **Signals are merged.** A bit that is set again before it is delivered stays set. A delivered bit means the signal was sent at least once since the last delivery, not how many times.
- **The word is saved in checkpoints** with the rest of the process record, as the `Restore_Notice` flag is now.

### 2. Named signals and the process's own signals

Bits 0 to 15 are named signals, defined by the system. Bits 16 to 63 belong to the process, which gives them their meaning.

| Signal | Kind | Effect |
|---|---|---|
| `Kill` | kernel | The kernel ends the process, as `Kill_Process` does now. |
| `Suspend` | kernel | The kernel stops scheduling the process. |
| `Resume` | kernel | The kernel schedules a suspended process again. |
| `Interrupt` | urgent | Ctrl-C: stop what you are doing (section 4). |
| `Restored` | ordinary | The process has been restored (section 6). |

The other named bits are reserved.

### 3. Kernel signals: the process never knows

`Kill`, `Suspend` and `Resume` are carried out by the kernel when they are sent. They never set a bit in the word, and the process can't see, handle or refuse them.

- **A suspended process is not scheduled,** whatever state it is in. If it was blocked in a call and the reply arrives, it stays suspended until it is resumed, and then sees the reply as usual.
- **Suspension is saved in checkpoints,** so a process suspended at a checkpoint is still suspended after a restore.
- **Other processes see no difference** between a suspended process and a slow one. A call to it waits.

### 4. `Interrupt`: acted on as soon as the process is scheduled

`Interrupt` must take effect at once, not when the process next happens to receive.

- **By default, it ends the process.** When the process is next scheduled, the kernel ends it, as for `Kill`.
- **A process can handle it instead,** by telling the kernel so (a kernel operation on its own process). Then, when the process is next scheduled:
  - if it is blocked in an invocation, the invocation is cancelled and returns a new error, `Interrupted`;
  - otherwise, its next invocation returns `Interrupted` without being made.
- **The run-time system raises an exception** for `Interrupted`, so in Ada it is handled like any other exception, and a generated command main (ADR 0002) turns it into `Fail`.
- **A handling process that is computing** without invoking anything sees `Interrupted` only at its next invocation. The shell can still use `Kill` on a job that doesn't stop. Handlers that break into running code would close this gap, and are an open question.

### 5. Ordinary signals: delivered at the next receive

`Restored` and the process's own bits are delivered when the process next receives, on any endpoint.

- **One message carries every pending bit,** in its first word, and clears them. It comes from the kernel and has no reply cap, as interrupts do.
- **It comes before any queued message or blocked sender,** as the restore notice does now (ADR 0004).
- **`Rose.Server` dispatches each bit** to the handler registered for it, and ignores bits with no handler. A process that registers none sees no change.
- **A process blocked in a call doesn't see them** until the call returns and it next receives.

### 6. Signal caps

- **A signal cap names a process and one bit.** It is a new cap type: the process's object id in the payload and the bit in the header, which fits the existing cap layout. Invoking it sets the bit, or for a kernel signal, carries it out. It never blocks and has no reply.
- **A process makes caps for its own bits,** through a kernel operation on its own process. It gives them to whoever should signal it: the timer, or a driver signalling `net`.
- **Caps for named signals come with the process cap.** Whoever holds a process's cap, usually the process that launched it, can make its `Kill`, `Suspend`, `Resume` and `Interrupt` caps. Exec gives petal those of each job it launches.
- **They can be derived and revoked** like any other cap (ADR 0001).

### 7. The timer and the restore notice use signals

- **`Set_Timer` is given a signal cap.** The timer invokes the cap it is given when the time is up. With a signal cap, an expiry can't be lost. The timer's interface doesn't change, but it should refuse caps that aren't signal caps, so the old, lossy path goes away.
- **`Restored` replaces the `Restore_Notice` flag.** The kernel sets the `Restored` bit on every process that restore resumes, and `Rose.Server` calls the process's `Restored` handler. A process that implements `Restorable` sees no change. ADR 0004's guarantee holds: the notice arrives before anything else the process receives.
- **`Kernel_Process.Notify` is removed.** Signal caps replace it.

## Consequences

**Benefits**
- A process can tell another that something happened without waiting for it, and the signal can't be lost.
- Timer expiries are never lost.
- The shell can stop, suspend and resume a job, and a job can't refuse to be killed or suspended.
- The restore notice stops being a special case.
- Sending a signal is a capability, so only the processes given a cap can signal a process, and the cap can be revoked.

**Costs**
- **Kernel work:** the signal word, a new cap type, cancelling a blocked invocation, and suspension in the scheduler.
- **Signals carry no data.** A process that uses one bit for several things, such as many timers, must work out which one happened.
- **48 bits for each process's own use.** A process with more sources than that must share bits between them.
- **Cancelling an invocation is new.** A server may already be working on the cancelled request, and its reply then has nowhere to go. The kernel must discard it safely.

## Alternatives considered

- **A pending bit for each endpoint.** A signal would be a message to an endpoint, kept if the receiver is busy. This needs state for each endpoint, and the receiver can't see all pending signals at once.
- **Counting signals.** A counter instead of a bit says how many times a signal was sent. It costs more space, and the senders that need a count can say so in a call.
- **Suspension that the process can see.** A process told that it is being suspended could refuse or delay it. Suspension is the shell's decision, not the job's.

## Open questions

- **Handlers that break into running code.** The scheduler could check a process's urgent signals each time it schedules the process, and act there: save the process's state and start it at a handler, as Unix signals do. A process computing without invoking anything would then see `Interrupt` straight away. But servers run without tasking, so a handler that runs in the middle of other code needs a run-time system that copes with it, and the kernel needs a way to return to the interrupted code. For now, cancelling the current invocation (section 4) covers the common case.
- **Hardware interrupts as signals.** An interrupt could set a bit instead of being sent as a message. That would remove the one-message slot that ADR 0004 works around, but a driver would lose the interrupt code that comes with the message.
- **Masking.** Should a process be able to hold off its own signals for a while, for example while it updates state that a handler reads?
- **Which caps petal gets.** Exec could give petal every named signal cap for a job, or only `Interrupt` and `Kill`, with `Suspend` and `Resume` for job control later.

## Follow-up work

- Kernel: the signal word, signal caps, and delivery of ordinary signals at receive, saved in checkpoints (new issue).
- Kernel: `Kill`, `Suspend` and `Resume` as signals, and suspension in the scheduler (new issue).
- Kernel and run-time system: `Interrupt`, the `Interrupted` error, cancelling a blocked invocation, and the exception (new issue).
- `Rose.Server`: dispatch signal bits to handlers (new issue).
- `Restored` as a signal, replacing the `Restore_Notice` flag (new issue).
- Timer: accept only signal caps (new issue).
- Remove `Kernel_Process.Notify` (new issue).
