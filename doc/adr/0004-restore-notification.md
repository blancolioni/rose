# 4. Telling a process that it has been restored

- **Status:** Accepted
- **Date:** 2026-10-26
- **Related:** #268 (this decision), #269 (persistent console, the first user), #102 (restore at boot), #114 (kernel and memory server restore operations)

## Context

Restore (#102) brings back the persistent processes as they were at the last checkpoint: their kernel tables, their caps, their memory and the interrupts they handle. A restored process continues from where it was, and it is not told that anything happened. For most processes that is right, because everything they depend on was restored with them.

Some processes depend on state that a checkpoint can't hold:

- **Hardware:** a reboot resets devices, so a driver's keyboard controller, disk controller or timer is no longer set up the way the driver left it.
- **Direct mappings:** a page that the kernel maps straight into a process, such as the VGA text page through `Identity_Map_Page`, isn't in the memory server's tables. The restored process no longer has it, and touching it is a page fault that the memory server can't satisfy.
- **The screen:** what was on the screen at the checkpoint is gone. The screen shows the messages of the boot that did the restore.

Such a process has to find out that it has been restored, and it has to find out before it handles anything else: a driver must set up its device before it handles the device's next interrupt.

### What restore does today

Restore recreates each process in the kernel, not yet runnable, then its memory, then the interrupts. After init replies, restore calls `Resume_Process` for each process. A process that was blocked in a system call makes that call again when it runs. So most processes, servers in particular, are back in a receive almost immediately.

The kernel already has a way to hold a message for a process that isn't receiving: a single queued message, used for interrupts that arrive while a driver waits for a reply. That slot holds one message, and a driver that is both restored and sent an interrupt would need two.

## Decision

### 1. A `Restorable` interface

A process that needs to know about restores implements a new interface:

```ada
interface Restorable is
   procedure Restored;
end Restorable;
```

It attaches the interface to its server context like any other. `Rose.Server.Register_Handler` creates the endpoint when the interface is attached, so the endpoint exists, and is saved in the checkpoint, from then on.

### 2. The kernel delivers `Restored`

- When restore resumes a process (`Resume_Process`), the kernel looks for its `Restorable.Restored` endpoint. If it has one, the kernel sets a flag on the process (`Restore_Notice`).
- The next time the process receives, on any endpoint or on that one, the kernel clears the flag and sends `Restored` to it. The process blocks in its receive as usual, and the notice is delivered the way any other sender's message is. It comes before any queued message and any blocked sender.
- The message comes from the kernel and has no reply cap, as interrupts do.

The flag is separate from the queued message, so it can't collide with an interrupt that is queued for the same process.

### 3. Restore itself doesn't change

Restore already resumes each process. It doesn't need to know which processes are restorable, and it gets no new caps.

## Consequences

- **A process that doesn't implement `Restorable` sees no change.**
- **The notification comes once per restore.** If a checkpoint is taken while a notification is still pending, the flag is saved with the process's other flags. A restore from that checkpoint sets it again, so the process is still told only once.
- **The handler runs before anything else the process receives,** so a driver can set up its device before its next interrupt, and the console can redraw before it writes again.
- **It doesn't make a blocked call safe.** A process that was blocked in a call still makes the call again, as before. The notification arrives at its next receive, after that call returns.
- **The kernel depends on another interface's endpoint id** (`Rose.Interfaces.Restorable`), as it does for `Kernel_Log` and `Memory`.

## Alternatives considered

- **Restore sends the notification itself.** Restore would need a cap to each restored process's endpoint, and only init can create those. It would also have to know which processes are restorable. The kernel already has the endpoint table.
- **Use the queued message slot.** This is simpler, but a driver with an interrupt pending would lose one of the two messages.
- **Something specific to the console.** For example, init could find the console after a restore by a fixed object id and tell it to redraw. Drivers need the same notification, and fixed object ids would add another dependency on the boot order.
