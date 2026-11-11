# 15. Process ownership: ending a process ends what it launched

- **Status:** Accepted
- **Date:** 2026-10-29
- **Amended by:** #750, where the store, not mem, records which bank is each process's own, a process asks for its bank rather than starting with it, `Launch`'s bank is a plain cap, and the store tells mem of processes to end by a send that doesn't wait (sections 1, 2 and 6)
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands, sections 8 and 9), [ADR 0006](0006-signals.md) (signals, section 3), [ADR 0014](0014-banks.md) (banks), #424 (end a child process), #346 (delete files and directories), #142 (background job handles), #320 (reuse process table entries)

## Context

A process that launches another can end it, but nothing ends the processes that the child launched in turn. #424 needs an answer before the directory server can delete entries (#346), and the shell needs the same answer for jobs (#142).

### What exists today

- **Launching.** A process calls `Launch.Launch` on a launch cap, which elf serves. Elf creates the kernel process, registers it with mem (`Memory.New_Process`), and returns the `Process` cap that mem serves. A launch cap names the program to run, not the caller, so elf doesn't know who launched what.
- **Ending a process.** `Process` inherits `Cap.Destroy`. Mem implements `Destroy` and `Exit_Process` in the same way: it frees the process's pages and segments, and deletes the kernel process. Whoever holds the `Process` cap can therefore end the process, and a process can end itself. Caps that others hold for the ended process then fail cleanly (#320).
- **Nothing is recorded about descendants.** Neither mem nor the kernel records who launched a process. When a process ends, the processes it launched keep running, and the caps for them that it held are lost. Nothing can then end those processes.
- **Long-lived processes are launched by short-lived ones.** Init launches the servers and the first user's shell, and then exits.
- **Kill on the kernel process cap.** ADR 0006 (section 3) makes kill, suspend and resume operations on the process cap, which the kernel carries out. A kernel kill alone doesn't reach mem's records.

### What's needed

- **Ending a process ends what it launched,** recursively, unless a process was explicitly launched to run independently.
- **It must work without the cooperation of the process being ended.** A process that is killed, or is misbehaving, runs no clean-up code.
- **Running independently must be deliberate.** ADR 0002 (section 8) distinguishes a background job (`&`), which belongs to the session, from a job that outlives the session, which needs an explicit capability.
- **Ended processes stay ended** across a restore.

## Decision

### 1. A process depends on its own bank

- **`Launch` takes a bank.** The caller passes the bank to launch against. Usually that's its own bank, but it can be any bank it holds a cap for.
- **Each launch creates a bank for the new process,** a sub-bank of the bank it was launched against. Elf reserves the process's storage from it (ADR 0014, section 4), and the process receives a cap for it when it starts.
- **The process depends on its bank.** Mem registers the process with the process's bank as a dependent object (ADR 0014, section 3).
- **By default, a process launches against its own bank,** so what it launches belongs to it: each child's bank is a sub-bank of the parent's.

### 2. Ending a process destroys its bank

- **`Destroy` on a `Process` cap, `Exit_Process` and the kill operation of ADR 0006 all destroy the process's bank.** Mem holds a cap for each process's bank, and destroys it when it ends the process.
- **Destroying the bank ends the subtree.** The store destroys the bank's sub-banks, deepest first. For each process that depends on one of them, the store invokes mem, which ends that process. The storage of all of them is released.
- **Mem and the store orchestrate ending, not the processes.** Nothing in the processes being ended has to run, so ending works for a process that is blocked, faulting or misbehaving.

#### Who keeps the link between a process and its bank (amendment, #750)

Amended by #750. Mem can't hold a cap for each process's bank, as section 2 has it. Mem is a boot module: it isn't restored, and at a restore it has its tables back as data, but none of its caps. The kernel has no way to say which of the store's banks a cap names, so mem couldn't even record which bank it had. So the store keeps the link, as data in its bank table, which its checkpoint saves (ADR 0014, section 7).

- **`Process_Banks`,** an interface of the store's that only mem holds. Init gives it to mem at every boot, once the store is running, because mem is launched first.
  - **`Create_Process_Bank (Parent, Process)`** makes a sub-bank of the bank with index `Parent` (`Bank.Index`), and records that it is `Process`'s own.
  - **`Process_Bank (Process)`** gives the cap for it again.
  - **`End_Process (Process)`** destroys it, with the banks below it.
  - **`Take_Ended`** gives the processes whose own banks were among them, one at a time.
- **Launching (section 1).**
  - Elf gives the launch's bank to `Memory.New_Process`, which makes the process's own bank.
  - Elf then asks the new process's `Process.Bank` for it, and reserves the process's storage from it.
- **Ending (section 2).** `Destroy` and `Exit_Process` end the process in mem, then call `End_Process`.
- **Ending the processes below.**
  - When the store destroys a bank that is some process's own, by `End_Process` or by `Bank.Destroy`, it queues that process.
  - The store then sends to mem's `Processes_Ended`, without waiting. The kernel queues one message for each process, and a second one replaces the first, but that is all a doorbell needs: mem takes every queued process each time it is told.
  - Mem ends each one as it does for `Destroy`, without calling `End_Process`, since the bank is gone.
  - Mem's boot-module server has no signal handlers, which a dependent object of ADR 0014 uses, so mem isn't a dependent of the banks.
- **A process asks for its own bank, rather than starting with it (section 1).**
  - `Process.Bank` gives it, and `Rose.System_Calls.Own_Bank.Cap` asks once and keeps it.
  - A cap in a fixed place in every process's cap table would have moved the caps that every driver expects.
- **`Launch` and `Launch_Command` take the bank as a plain cap,** not as an `interface Bank`, so that every program that launches another doesn't need the `Bank` interface's client.
  - **A null cap means the launcher's own bank:** exec's, the root bank until init gives it a system bank (#450). Init launches its servers that way for now.
  - Programs launch against their own bank: petal, login, auth, the directory server and the self tests.
- **The kernel's kill operation (ADR 0006, section 3)** doesn't tell mem, so a killed process's bank isn't destroyed yet. That is a follow-up.

### 3. Running independently means launching against another bank

- **A process launched against a bank that the launcher doesn't own is independent of the launcher.** Its bank is not under the launcher's, so it ends when that other bank is destroyed, not when the launcher ends.
- **The grant to run independently is a bank cap.** The right to "outlive the session" in ADR 0002 (section 8) is a cap for a bank outside the session. You can't detach a process by accident, because you need to hold another bank first.
- **Init launches the system's long-lived servers against the system bank** (ADR 0014, section 2), not its own, because init exits when it has started them.

### 4. Jobs in the shell

- **A command runs against petal's bank by default,** so it ends when it is cancelled or when petal ends.
- **A background job (`&`) is still a job of the session.** It runs against petal's bank too. The handle from #142 holds the job's `Process` cap, so the job can be cancelled.
- **A job that outlives the session runs against a bank outside the session,** such as the user's bank. Petal can launch there only if the session holds that bank's cap.

### 5. Graceful ending is the process's choice

- **Destroying a bank is abrupt.** The processes that depend on it end without notice.
- **A process that wants an orderly shutdown asks for one first.** For example, it sends ADR 0006's `Terminate` to the processes it launched, waits for them, and then exits. The guarantee doesn't depend on that: whatever is left ends when the bank is destroyed.

### 6. Persistence

Banks and their dependent objects are saved in the store's checkpoint (ADR 0014, section 7), and mem's process table is saved in mem's. A destroyed bank and its processes are gone from both before the next checkpoint, so they don't come back on restore.

Amended by #750: which process owns a bank is saved in the store's bank table, so nothing holds a cap to a bank across a restore. The store's queue of processes to end isn't saved, because mem empties it as soon as it is told. A checkpoint taken in between would restore a process whose bank is gone: it can still run, but reserving storage fails. Ending it at the restore is a follow-up.

## Consequences

- **#424 becomes small for the directory server.** It already gets each entry's `Process` cap from `Launch`. It keeps the cap in the entry, and calls `Destroy` when the entry is deleted (#346). A directory's files are launched against the directory's bank, so destroying a directory also ends its files, if there are any.
- **The kill operation in ADR 0006 (section 3) destroys the process's bank.** A kernel kill that skipped mem would leave mem's records and the process's descendants behind. This amends ADR 0006.
- **`Launch` changes signature,** and every caller passes a bank. Elf creates the process's bank from it, and passes that to mem.
- **Every process receives one more standard cap,** for its own bank.
- **Init must launch servers against the system bank.** Launching them against its own bank would end them when init exits.
- **A server launched by one process and used by others ends with its launcher.** A shared service must be launched against a bank that outlives its users.
- **Ending is a hard kill for every process under the one that was ended.** Processes that need to tidy up rely on the process above them to ask first (section 5).

## Alternatives considered

- **The process being ended ends its own children.** For example, it could handle `Terminate` by ending each process it launched. This fails exactly when it matters, for a process that is killed or misbehaving, and it relies on every process getting it right.
- **The launcher keeps each child's `Process` cap and ends it.** This is today's model, and it doesn't recurse. One process that loses or never keeps a cap leaks a whole subtree, as the directory server does now.
- **Elf keeps a process tree.** This would turn the launch server into a central process table, and elf can't see who called `Launch` anyway.
- **The kernel records an owner for each process,** and a kernel kill ends the owner's subtree. This puts policy in the kernel, and the kernel can't see the logical parent either: elf creates every process. It would also duplicate the tree that banks already have.
- **A flag on `Launch` to detach.** A flag lets any process create processes that outlive it. A bank cap makes detaching a right that has to be granted.

## Open questions

- **Ending a subtree gracefully.** Should there be an operation that sends `Terminate` to every process under a bank, deepest first, and destroys the bank once they have exited, or after a timeout?
- **Who holds the system bank cap after init exits?** Probably nobody but the store, so system servers end only when the system does.

## Follow-up work

- Done: `Launch` takes a bank, elf reserves from the bank it is given (#449), and mem makes each process's own bank, ends a process whose bank was destroyed, and destroys a process's bank on `Destroy` and `Exit_Process` (#750). A process asks for its bank through `Process.Bank` (#750).
- Mem: destroy the bank of a process that the kernel's kill operation ends.
- Init launches servers against the system bank (#450).
- #424 (done): the directory server keeps each entry's `Process` cap, and destroys it on delete (#346).
- Petal gives the commands it runs a bank (#517). The self tests launch against a sub-bank and destroy it, so that a run leaves nothing behind (#724).
- #142: background jobs run against petal's bank, and a job that outlives the session runs against a bank outside the session.
- ADR 0006: note that the kill operation destroys the process's bank.
