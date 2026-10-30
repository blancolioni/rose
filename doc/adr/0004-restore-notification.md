# 4. Telling a process that it has been restored

- **Status:** Accepted
- **Date:** 2026-10-26
- **Related:** #268 (this decision), #269 (persistent console, the first user), #102 (restore at boot), #114 (kernel and memory server restore operations)
- **Amended by:** #519, where a blocked call is made again only if the server has no record of it (section 4)

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

### 4. A blocked call is made again only if the server has no record of it

Amended by #519. Making every blocked call again, as restore first did, is right when the server has no record of the call, and wrong when it has one: the server then handles the request twice. A restored server has a record of every call that reached it. It holds a reply cap for the caller, and it may be handling the request or holding it for later, as the console holds a `Read`.

When restore resumes a process that was blocked in a call, the kernel decides from what the process was waiting for:

1. **The caller is in a restored server's waiting list.** The message wasn't delivered. The kernel puts the caller back in the list, and the server gets the message at its next receive. The checkpoint holds the list.
2. **A restored process holds a reply cap that names the caller.** The request was delivered. The caller keeps waiting for the reply, and the server answers it when it's ready. A reply cap names its caller by object id, so it is still good after a restore.
3. **Otherwise,** the call was to a boot module or a kernel cap. A boot module starts fresh at each boot and has no record of the call, so the caller makes it again, as before.

The first rule comes before the second, because a caller that is still waiting for the server to receive already has a reply cap in the server's table. A queued message is restored with its process, so that a call whose message was queued isn't lost under the second rule.

## Consequences

- **A process that doesn't implement `Restorable` sees no change.**
- **The notification comes once per restore.** If a checkpoint is taken while a notification is still pending, the flag is saved with the process's other flags. A restore from that checkpoint sets it again, so the process is still told only once.
- **The handler runs before anything else the process receives,** so a driver can set up its device before its next interrupt, and the console can redraw before it writes again.
- **A blocked call is handled once.** A call that a restored server holds is answered by that server, and isn't made again (section 4). A call that a restored server hasn't received yet waits for it, as before the checkpoint.
- **A process blocked in a call is told about the restore after the call returns,** at its next receive.
- **The kernel depends on another interface's endpoint id** (`Rose.Interfaces.Restorable`), as it does for `Kernel_Log` and `Memory`.

## Alternatives considered

- **Restore sends the notification itself.** Restore would need a cap to each restored process's endpoint, and only init can create those. It would also have to know which processes are restorable. The kernel already has the endpoint table.
- **Use the queued message slot.** This is simpler, but a driver with an interrupt pending would lose one of the two messages.
- **Make every blocked call again.** It needs no decision, and it is what restore first did. A restored server that holds the request then handles it twice, and its reply to the first request is taken as the reply to the second (#519).
- **Fail every blocked call, as when a server dies (`Closed`).** The caller would decide whether to make the call again. Every caller would have to handle it, although a restored server can answer the call itself.
- **Something specific to the console.** For example, init could find the console after a restore by a fixed object id and tell it to redraw. Drivers need the same notification, and fixed object ids would add another dependency on the boot order.
