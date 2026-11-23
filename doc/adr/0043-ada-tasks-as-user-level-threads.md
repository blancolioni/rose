# 43. Ada tasks as user-level threads

- **Status:** Proposed
- **Date:** 2026-11-23
- **Related:** #362 (this decision), #363 (tasking in the userland RTS), #1066 (user-mode process scheduling), #1068 (open calls), #1069 (scheduler library), #1070 (preemption upcall), #708 (signals), #512 (pausable calls), #519 (blocked calls at a restore), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0006](0006-signals.md) (signals), [ADR 0008](0008-install-boot-and-pager-path.md) (install boot and pager path)
- **Amends:** [ADR 0006](0006-signals.md), section 5 (a process may have more than one open call) and its open question on handlers that break into running code (section 5 here)

## Context

The userland RTS has no tasking. `rts/user/restrictions.adc` sets `No_Tasking`, and a program can't use tasks, protected objects or `delay`. Programs that wait for more than one thing at a time, such as a server that also runs a timer, or a REPL that reads keys while it evaluates, have to be written as one receive loop.

A Rose process has one flow of control. Ada tasks need several in one address space, and they can be provided in two ways:

- **Kernel threads:** several flows of control share one address space and cap table. The kernel's process table, its scheduler and the checkpoint format all change, since each thread's registers and state must be saved and restored, and mem needs a stack segment for each.
- **User-level threads:** the RTS keeps the tasks and switches between them inside the process. The kernel sees one process.

### What exists today

- **A call blocks the whole process.** A call is an invocation with `Send`, `Receive` and `Create_Reply_Cap`. The kernel marks the caller `Wait_Reply` and blocks it until the reply comes.
- **A process has at most one open call.** A reply is delivered only to a caller that is blocked and marked `Wait_Reply`. A reply to a process that isn't waiting is dropped. The record of a lent buffer (`Lent_Buffer_Page`, `Pinned_Page`) is also kept once for each process.
- **Pausable calls are planned but not built.** ADR 0006, section 5 (amended by #512) lets a call return `Signalled` while it stays open, and the process collects the reply later with `Wait_Reply`. It allows one paused call per process.
- **Timers.** The kernel has one timeout, held by the timer service, which multiplexes it for everyone else. With ADR 0006, section 7, `Set_Timer` invokes a signal cap, so an expiry can't be lost. Only the pager path uses the kernel's timeout and tick caps directly (ADR 0008).
- **Preemption between processes** is the kernel's. Nothing can interrupt a running process and run code inside it. ADR 0006 leaves this as an open question: "handlers that break into running code".
- **Restore.** A process's memory and kernel record are saved in a checkpoint. A call that was blocked at the checkpoint is waited for again or made again by ADR 0004, section 4 (#519).

### User-mode scheduling

ADR 0003 plans user-mode scheduling and private process tables, so that a directory and its child servers run in one kernel process. #1066 is that work. A process scheduler in userland needs the same mechanisms as tasks: a way for one flow of control to wait for a reply while the others run, a timer, and preemption. It also needs isolation between hosted processes, which tasks don't.

## Decision

### 1. Tasks are user-level threads

- **The userland RTS schedules Ada tasks** inside the process. The kernel has no threads, and its process table and scheduler don't change for tasking.
- **Tasks are saved with the process.** A task's stack, registers and state are in the process's memory, so a checkpoint saves them at no extra cost, and a restore brings them back.
- **Boot modules have no tasking.** The kernel-target RTS keeps `No_Tasking`, `No_Protected_Types` and `No_Delay`.
- **The scheduler is a library** that the tasking RTS uses, and that a process scheduler (#1066) can use too: run queues, switching, waiting for the next reply or message, timeouts. Nothing in it is specific to Ada.

### 2. Profile: Jorvik first

- **The first profile is Jorvik:** tasks declared at library level, protected objects with any number of entries, `delay` and `delay until`, `Ada.Real_Time`, and `FIFO_Within_Priorities`.
- **Full tasking comes later, if it is needed:** dynamic tasks, rendezvous, `abort`, asynchronous select, and the task hierarchy. Nothing here rules them out.
- **One CPU.** The tasks of one process run on one CPU at a time. Multiprocessor support (#191) is a separate question.

### 3. A task waits for a reply without blocking the process

The kernel lets a process have several open calls, each with a call number.

- **An open call.** A call made with a new invocation flag returns at once, with a call number, instead of waiting for the reply. The server receives it, holds its reply cap and answers it, as for any call.
- **Replies are held for the caller.** When the reply comes, the kernel holds it, with its call number, until the caller receives.
- **One receive for everything.** A receive returns the next of: a held reply, with its call number; a message on one of the process's endpoints; or its pending signals (ADR 0006). The restore notice still comes first (ADR 0004).
- **A limit for each process.** The number of open calls is bounded by the kernel. A call over the limit fails with an error, which the RTS raises as an exception in the calling task.
- **Each open call has its own lent buffer.** The kernel keeps the buffer and pinned pages for each open call, not once per process.
- **Open calls are saved in checkpoints,** with any reply held for them. After a restore, each open call follows ADR 0004, section 4, as a blocked call does now: waited for again if a restored server holds it, made again if it went to a boot module.
- **A call without the new flag is unchanged.** A process that doesn't use open calls sees no change.

The RTS makes a call from a task as an open call, records the call number against the task, and switches to another ready task. When no task is ready, the scheduler receives, and the reply makes its task ready again.

**This amends ADR 0006.** A pausable call becomes an open call that the process was waiting on. The limit of one paused call per process goes, and so does the separate `Wait_Reply` operation: the process waits for a paused call's reply with the ordinary receive, by its call number.

### 4. Time: the timer service, one timeout for each process

- **The scheduler asks the timer service** for one timeout at a time, for the earliest `delay until` deadline among its tasks, and changes it when an earlier one appears.
- **The expiry is one of the process's own signal bits,** through a signal cap (ADR 0006, section 7), so it can't be lost.
- **`Ada.Real_Time.Clock` is the kernel's tick count**, through the RTS. It never goes backwards across a restore. It carries on from its value at the checkpoint, so the time the system was down doesn't count, and a `delay` doesn't end early because of a restore.
- **`Ada.Calendar` is the wall clock** (#494). A `delay until` on a `Calendar` time that passed while the system was down ends as soon as its task is next run.

### 5. Preemption: the target, through a kernel upcall

Preemption between the tasks of a process is the target. A task that computes without blocking must not hold up the others, and a process scheduler (#1066) can't let one hosted process hold up its neighbours.

- **The kernel delivers an upcall.** A process registers an upcall entry and a save area in its own memory. When an upcall is due and the process is about to run in user mode, the kernel writes the process's registers to the save area and starts it at the entry, with upcalls held off.
- **The process sets its own quantum.** It tells the kernel how many ticks it may run before an upcall is due. The kernel counts only the ticks in which the process is running, so this needs no timer service call. A quantum of zero turns it off, which the scheduler does when only one task is ready.
- **Urgent signals use the same upcall.** ADR 0006's `Interrupt` can reach a process that is computing, which answers that ADR's open question on handlers that break into running code.
- **The RTS holds off preemption itself.** While a task is in a protected action, or the scheduler is changing its own state, the RTS sets a flag in the process's memory. An upcall that finds the flag set records that a switch is due and returns to the interrupted code at once. The switch happens when the flag is cleared. This is how ceiling locking works on one CPU.
- **The upcall is saved in checkpoints** as part of the process's memory and its kernel record. A process can be checkpointed in the middle of an upcall like anywhere else.

**Preemption comes in a second stage.** The first build of #363 switches tasks only when a task blocks: at a call, a protected entry wait, `delay` or `delay until`. It needs open calls (section 3) and no upcall. The rules for protected actions are the same in both stages, so adding preemption changes no program.

### 6. Stacks and registers

- **Each task has its own stack** in the process's memory, sized by `Storage_Size`, or a default. Library-level tasks have their stacks allocated at elaboration.
- **A switch saves the task's registers and FPU state** in its task control block. The kernel already saves the FPU state for each process (#170), and doesn't need to know which task is running.

### 7. Restore

- **Tasks come back as they were.** A ready task is ready, a task waiting on a protected entry still waits, and a task in a call follows section 3.
- **The restore notice goes to the scheduler,** which receives it first (ADR 0004, ADR 0006 section 7). It calls the program's `Restored` handler, if it has one, before any task runs again.
- **Timeouts are set again.** The timer service's pending timeouts are saved with it (ADR 0008), so the scheduler's timeout survives the restore. The scheduler checks its deadlines against `Ada.Real_Time.Clock` after the restore anyway.

## Consequences

- **No kernel change for threads.** The kernel's process table, scheduler and checkpoint format don't grow per task.
- **Kernel work for open calls:** held replies, call numbers, lent buffers for each open call, and the receive that returns any of them, all saved in checkpoints. This replaces the pausable-call work in ADR 0006, and serves signals, tasks and #1066.
- **Kernel work for the upcall:** registration, a per-process quantum, the save area, holding off upcalls, and returning to the interrupted code. It is the second stage, after #363's first build.
- **Every blocking operation goes through the scheduler.** RTS code that makes a call directly, as all of it does now, has to make it through the scheduler once tasking is on, or it blocks every task.
- **Jorvik's dispatching is only partly met before the upcall.** A higher-priority task that becomes ready waits for the running task to block.
- **The configuration pragmas go.** `No_Tasking` leaves `rts/user/restrictions.adc`, and `Suppress_Standard_Library` changes in the user `System`.

## Alternatives considered

- **Kernel threads.** Blocking calls would work as they do now, and preemption would come from the kernel's scheduler. But each thread would be kernel state to schedule, save and restore, the checkpoint format would change, and the kernel process limit would fill faster, which is what #1066 is meant to relieve.
- **User-level threads, and kernel threads later for more than one CPU.** Possible, and not ruled out, but #191 doesn't need it yet.
- **Cooperative switching only, for good.** No kernel upcall, and protected objects are atomic for free. But a busy task holds up the others, and a process scheduler built on the same library (#1066) couldn't stop one hosted process from holding up the rest.
- **One RTS task makes every call, for the others.** It needs no kernel change, but only one call can be open at a time, so a task waiting for a slow server holds up every other task's calls.
- **Pausable calls as ADR 0006 has them.** One paused call per process isn't enough for more than one task in a call.
- **Preemption by the timer service.** The scheduler would set a timeout for each quantum. That is a call to the timer service for every slice, and an expiry still couldn't interrupt a task that is computing without an upcall.

## Implementation

- Kernel: open calls, call numbers, held replies, a lent buffer for each open call, the receive that returns the next reply, message or signal, and saving them in checkpoints (#1068). Replaces the pausable-call issue from ADR 0006.
- Scheduler library: run queues, switching, waiting for the next event, one timeout from the timer service (#1069).
- Userland RTS: Jorvik tasking on the scheduler library, switching at blocking points (#363).
- Kernel and RTS: the preemption upcall, the quantum, and holding off preemption in protected actions (#1070).
- User-mode process scheduling, on the same library and kernel mechanisms (#1066).
