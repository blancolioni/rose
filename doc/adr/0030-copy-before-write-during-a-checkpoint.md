# 30. Copying a page before it is written during a checkpoint

- **Status:** Proposed
- **Date:** 2026-11-09
- **Related:** #795 (this decision), #807 (its duplicate), #543 and #714 (replies held during a checkpoint), #314 (buffers of calls in progress), [ADR 0005](0005-persistent-console.md) (device pages), [ADR 0007](0007-networking.md) (networking), [ADR 0008](0008-install-boot-and-pager-path.md) (the pager path), [ADR 0025](0025-shared-regions.md) (shared regions), [ADR 0028](0028-paging-to-the-log.md) (paging to the log, which this amends)

## Context

A checkpoint stops every persistent process that writes to its memory, from the moment the kernel enters checkpoint mode until it leaves (the book's *Persistence* chapter, *Checkpoint Mode*):

- **Writes.** `Disable_Page_Writes` write-protects every page that `mem` manages and that is writable, and marks it `Checkpoint_Read_Only`. A write to any managed page of a persistent process, including a clean one, sets `Checkpoint_Page_Fault`. The process waits until `Leave_Checkpoint`.
- **Replies.** A reply or message to a blocked persistent process is held (`Checkpoint_Reply_Held`, #543, #714). The kernel would copy it into the process's invocation record. That record is in the process's memory, and is saved after the kernel's tables, which already record the call.

A checkpoint takes 2.4 to 3.2 s on QEMU today. For most processes, the stop does no harm. For a process that has to keep up with hardware, it loses data, and #795 shows how:

1. The keyboard driver is in its `On_Event` call to the surface console. The surface console writes to its memory and is stopped, so the driver's reply is held, and the driver stays blocked.
2. The kernel queues one message for each process. Each key's interrupt after the first is dropped (`lost queued message`).
3. The keys themselves are lost in the PS/2 controller. QEMU's queues 16 bytes, and a key is 2 or 3 bytes. A real 8042 holds one byte.

The text console survives only because `vga-console` writes to VGA text memory, a device page, which a checkpoint doesn't protect. Making the driver's delivery non-blocking isn't enough: the driver itself writes to its stack and buffer, and is stopped by its first write.

Any driver of input that the system doesn't control has the same problem.

- **The network is the worst case** (ADR 0007).
  - The card's driver and `net` are both persistent. Either one stops at its first write, so frames go unread, the card's receive ring fills, and the card drops whatever arrives.
  - TCP recovers from the loss, but a stop of seconds every 30 s means retransmissions and a collapsed congestion window, and UDP doesn't recover at all.
  - `net`'s TCP timers also run late by the length of the stop.
- **The others:** the mouse (same controller), serial ports, and audio input.

The stop can't be made safe by making checkpoints shorter. A checkpoint writes every page changed in 30 s, so its length grows with the work done, and any stop longer than the hardware's buffer loses data.

The constraint is the one in section 1 of ADR 0028: **a restore must see exactly the state of the last committed checkpoint.** The kernel's tables are a snapshot of the moment the kernel enters checkpoint mode, so every saved page must hold its contents at that moment.

KeyKOS and EROS solved the same problem the same way: a checkpoint marks the dirty objects copy-on-write, and processes run on while they are written.

## Decision

### 1. The pages in a checkpoint are fixed when it starts

A checkpoint saves the pages of persistent processes that are dirty when the kernel enters checkpoint mode. Those are the pages that `Disable_Page_Writes` marks `Checkpoint_Read_Only`, since `mem` maps a page writable only once it is dirty.

- **`mem` saves exactly that set.** `mem` may map a page for a persistent process while checkpoint mode is on, before its own step of the checkpoint has run. That page isn't saved by this checkpoint, even if `mem` maps it writable. It is dirty for the next one.
- **The saved contents of a page are its contents when the checkpoint started,** whoever writes to the page afterwards.

### 2. The kernel copies a page before its first write

When anything writes to a `Checkpoint_Read_Only` page during checkpoint mode, the kernel first copies the page's physical frame into a frame of its own.

- **The process stays on its own frame.** The copy is the checkpoint's, not the process's. The kernel then makes the page writable, and the write goes ahead.
  - No page table entry has to point anywhere new, so `mem`'s tables still name the right frame.
  - A frame of a shared segment (ADR 0025) is mapped by several processes, and its copy serves all of them.
- **The kernel keeps a table of copies,** from each frame it has copied to its copy. A frame is copied at most once in a checkpoint.
- **Writes by the kernel itself are treated the same way.** A reply or message that the kernel copies into a persistent process's memory goes through the same check first. So the reply reaches the process's own frame, and the checkpoint keeps the frame as it was when the call was saved.
  - Replies no longer need to be held, and `Checkpoint_Reply_Held` goes.
  - Interrupts, replies and messages reach persistent processes during a checkpoint as at any other time.
- **The kernel does the copy, not `mem`.** `mem` is busy writing the checkpoint for most of it (section 4), and a write that waited for `mem` would wait as long as it does today.

### 3. The first write to a clean page

A clean page of a writable segment is mapped read-only, so that `mem` sees its first write (`Set_Writable`). During checkpoint mode, `mem` is busy, so such a write would wait for it. An idle driver's pages are clean, so its first key or frame during a checkpoint would wait.

- **`mem` marks such a page `Write_Tracked`,** in the spare bit of its page table entry (bit 11 on i686). The bit means that a write makes the page dirty, and is not a fault.
- **During checkpoint mode, the kernel handles that write itself.** The page isn't in this checkpoint (section 1), so it needs no copy. The kernel makes the page writable and records it as written (section 5).
- **Outside checkpoint mode, the write goes to `mem` as now.** Whether the kernel should always handle it, and save a call to `mem` for every first write, is left for later.

A write to a page that is read-only in its segment is still a fault, and goes to `mem`.

### 4. `mem` saves through the kernel

`mem` writes each saved page with a new kernel call, `Copy_Saved_Page`. The call takes the frame and `mem`'s page buffer.

- **The kernel copies the saved contents into the buffer:** the copy if it has one, otherwise the frame itself.
- **The kernel then marks the frame as saved, in the same call.** Later writes to it need no copy, and a copy it had goes back to the reserve (section 6).
  - The kernel isn't preempted inside a call, so the read and the mark can't be split by a write.
  - Two calls, one to find the frame and one to mark it saved, would let a process write in between and tear the saved page.
- **`mem` appends its buffer,** as it now appends a mapped frame (`Mem.Virtual_Map.With_Page`).

This costs one more copy of each saved page, from the frame into `mem`'s buffer.

### 5. The end of the copying

When `mem` has saved its private and shared pages, it calls `End_Page_Copies`.

- **The kernel stops copying**, and makes writable again every page that is still `Checkpoint_Read_Only`, as `Leave_Checkpoint` does today.
- **The kernel gives back the pages written during the checkpoint,** each as a process and a virtual page. This includes those written in place under section 3. `mem` marks each one dirty and writable in its tables, so the next checkpoint saves it.
- **The kernel gives back the frames of the reserve** that it didn't use.

The rest of the checkpoint, the commit, doesn't depend on the processes' memory. So for memory, checkpoint mode ends here, not at `Leave_Checkpoint`. `Leave_Checkpoint` still ends the checkpoint for `log` and `checkpoint`.

`mem` makes this call in the same request as its writing (`Mem.Checkpoints.Checkpoint`). So the written pages reach `mem`'s tables before it handles anything else, such as a fault, or a page-out under ADR 0028.

### 6. The reserve of frames

The kernel copies into frames that `mem` gives it beforehand, and never allocates physical memory itself.

- **`mem` keeps the kernel's reserve full.** It gives the kernel frames at boot, and refills the reserve after each checkpoint with `Add_Copy_Frames`. The reserve has to be in place before checkpoint mode starts, since a write can come at once.
- **A copy goes back to the reserve as soon as `mem` has saved its page** (section 4). The reserve only needs to cover pages that are written before `mem` reaches them, not every page a process writes.
- **`mem` chooses the size.** It starts at 64 frames, 256 KiB, which is many times the pages an input driver writes in a few seconds. It has to be measured with real workloads.
- **If the reserve is empty,** the writer waits, as every writer waits today (`Checkpoint_Page_Fault`). It waits until `mem` saves a copied page and so frees a frame, or until `End_Page_Copies`. The kernel logs `checkpoint copy reserve empty` once for each checkpoint.
- **The table of written pages is bounded too,** by one page of entries, which `End_Page_Copies` returns. If it fills, a writer waits in the same way.

So a checkpoint under heavy writing is no worse than today, and the usual case doesn't stop anything.

### 7. What still waits

- **A fault that needs `mem`**: a page that isn't mapped, or one that ADR 0028 has paged out, waits until `mem` has finished its step.
  - A driver that must keep up with hardware keeps its working set mapped: it writes to its buffers and stack once at start-up.
  - Under ADR 0028, `mem` must not page out the pages of such a driver. How a driver asks for that is left to the network work (#291 to #301).
- **A call to a persistent process that is itself waiting** waits with it. The rules of ADR 0007, section 2, already keep drivers from calling up the stack.

### 8. What doesn't change

- **Device pages** aren't managed by `mem`, and aren't protected or copied (ADR 0005).
- **DMA buffers must not be persistent pages.** A device writes memory without going through the page tables, so neither protection nor copying can see its writes. ADR 0007's drivers already use identity-mapped pages from the low memory that init reserves (#291). This ADR makes that a rule.
- **The buffers of calls in progress** stay writable during a checkpoint, as #314 has them. They aren't copied.
- **Restore is unchanged.** The saved pages and the kernel's tables are both from the moment the checkpoint started, which is what restore already assumes.
- **A failed commit** loses what it lost before. `mem` has cleared its dirty pages, so pages saved only in the failed entry are not saved again until they change. This ADR neither causes nor fixes that.

### 9. Interfaces

- **Kernel calls on `mem`'s kernel cap:**
  - `Add_Copy_Frames (Frames)`
  - `Copy_Saved_Page (Frame, Buffer)`
  - `End_Page_Copies`, which returns the written pages and the unused frames
- **Page table entries:** `Write_Tracked`, in bit 11 on i686. An architecture with no spare bit keeps the bit in a table of its own.
- **Removed:** `Checkpoint_Reply_Held`, and the waiting at `Leave_Checkpoint` of processes that are only waiting for a reply.
- **`Checkpoint_Page_Fault` stays,** for a writer that finds the reserve or the table of written pages full.
- **ADR 0028, section 10, is amended.** Persistent processes can change pages during a checkpoint. Their changes go to their own frames, and the copies come from the kernel's reserve, not from `mem`'s free pages. The pages they write go into `mem`'s table of changes at `End_Page_Copies`.

## Consequences

- **Persistent processes run through a checkpoint.** A key, a frame or a timer expiry is handled when it arrives, and isn't held for seconds.
  - #795 and #807 are fixed, as long as the drivers' pages are mapped (section 7).
  - The network can be persistent without dropping frames at every checkpoint.
- **Checkpoints can get longer without harm,** as the amount of changed memory grows. Only the reserve has to keep up.
- **The kernel copies pages,** which it hasn't done until now:
  - 4 KiB for each page written during a checkpoint before `mem` has saved it;
  - 4 KiB for each saved page, into `mem`'s buffer.

  Both are bounded by the pages a checkpoint saves.
- **Physical memory for the reserve** is taken from `mem`: 256 KiB to start with.
- **The kernel gains a table of copies and a table of written pages,** both bounded, and both empty outside a checkpoint.
- **Replies are no longer held,** which removes a special case from the kernel's reply path (#543, #714).
- **The book's *Persistence* chapter changes:** *Checkpoint Mode* and *The Sequence* describe copying instead of waiting.

## Alternatives considered

- **The kernel drains the PS/2 controller** (#795, option 2). The IRQ 1 handler reads port `0x60` into a ring buffer in the kernel, and the driver reads from that.
  - It is small, but it fixes one device by putting knowledge of it in the kernel, and the network needs the same thing again, with a card's ring.
  - It also leaves the keys stuck in the kernel until the checkpoint ends, so the console still freezes.
- **Input drivers aren't persistent** (#795, option 3). A driver that isn't checkpointed is never stopped.
  - Its listeners and its clients' caps would have to be rebuilt at every restore.
  - It doesn't help `net`, which holds clients' requests and its caps, and must be persistent. Nor does it help the consoles that the drivers call, which are stopped as well.
- **Shorter checkpoints** (#795, option 4). This narrows the window but doesn't close it (Context).
- **Copy-on-write the other way:** the process moves to the copy, and the checkpoint keeps the original frame.
  - The kernel would have to point the page table entry at a new frame, and `mem`'s tables would name the wrong frame until it was told.
  - A shared frame would need every process that maps it to move, and the kernel has no reverse map to find them.
  - Copying the checkpoint's side instead (section 2) leaves every mapping alone.
- **`mem` does the copying.** `mem` already owns physical memory and handles write faults. But it is busy writing the checkpoint for most of it, so a write that waited for `mem` would wait almost as long as today.
- **Copy every dirty page when the checkpoint starts.** This needs as many free frames as there are dirty pages, and copies all of them, where most are never written during the checkpoint. Copying before the first write copies only what is written.
