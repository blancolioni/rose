# 39. Boot services and persistence

- **Status:** Proposed
- **Date:** 2026-11-19
- **Related:** #1001 (this decision), #1000 (a persistent store), #967 (generations instead of rescinding derived caps), #997 (console output while paging), #998 (mem's mapped-page table), #914 (mem and the store deadlock), #862 (kernel handover), #831 (replaceable servers), #890 (bridge release), [ADR 0008](0008-install-boot-and-pager-path.md) (install boot and pager path), [ADR 0014](0014-banks.md) (banks), [ADR 0015](0015-process-ownership.md) (process ownership), [ADR 0028](0028-paging-to-the-log.md) (paging to the log), [ADR 0031](0031-updating-an-installation.md) (updating an installation), [ADR 0032](0032-replacing-a-running-process.md) (replacing a running process)

## Context

ADR 0008 split processes into two kinds. The **pager path** (`mem`, `store`, `partition`, `ata`, `log`, `checkpoint` and `restore`), with `init` and the boot console, is made of boot modules. They start afresh each boot, save the state that persistent processes depend on as data, and are given it back at a restore. Every other process is persistent.

The split keeps a fault, a checkpoint or a restore from waiting on a process that it would itself have to page in. That rule still holds. But putting the whole of each service on the pager path has costs that keep coming back:

- **No caps across a restore.** A boot module's cap table isn't saved. The store makes new caps for its regions and banks each boot, so it can't keep the derived caps it hands out, and can't rescind them later (ADR 0001). #967 uses a generation in the identifier instead. Mem can't hold a cap for each process's bank (ADR 0015, as amended by #750). Every interface that persistent processes call on a boot module needs its own restore story.
- **Fixed memory.** Boot modules take their memory from a reservation that the kernel makes before mem runs (`Boot_Module_Pages`). Every table they keep is sized at build time and added to it: mem's table of changes since the last checkpoint and the window it pages out through (#974), the store's tables. Mem's table of mapped pages is the exception: since #998, mem sizes it from its physical pages when it starts, and maps it from its own memory.
- **Nothing on the path may wait on a paged process.** A boot module that calls the console while mem is paging can deadlock, because the console calls a launched driver whose pages mem may have dropped (#914, #971, #997). The store's own memory must never fault. These rules hold for everything a boot module calls, and are easy to break.
- **Policy is on the pager path.** Which pages to drop, how banks are charged, how regions grow: all of it lives in boot modules, and changes with them. A boot module can't be replaced while it runs (ADR 0032), only by the update boot's new boot set (ADR 0031), which must still read every older checkpoint's data formats.

KeyKOS, EROS and Coyotos avoid this by putting the object store and the checkpoint in the kernel, and keeping everything else persistent. Rose keeps them out of the kernel, so the question is where the line goes inside userland.

## Options

### A. Make boot services persistent

A service such as the store, mem or the log becomes a persistent process, restored like any other.

- **Its image can't come from the store.** A persistent process maps its code from `exec`'s region, which is in the store (#1000). The image would have to be in the boot set, the log, or memory that is never paged.
- **Its memory must never be paged out,** since only it, or mem through it, could bring the memory back. That needs a pinned kind of persistent memory, saved at a checkpoint but never dropped.
- **Checkpoint mode write-protects persistent processes.** A pager-path service has to write during a checkpoint, so it would be an exception to that rule, with its own way to be consistent with the snapshot.
- **The restore order changes.** It has to run before anything faults on a page that it serves, so the restore must bring it back first, from data that doesn't need it.

### B. Hand boot servers over

A boot module serves until the restore is far enough on, then hands its role and its state to a persistent server, through the kernel's handover (#862) and the replacement protocol (ADR 0032). #1000 asks this for the store.

- **The boot module still exists,** and still has to restore the state that the persistent server takes over, so it keeps its data formats.
- **The persistent server has the problems of option A** from the handover on: its image, its memory, and checkpoint mode.
- **The handover itself** has to happen while faults are being served, and the persistent server's callers keep their caps across it.

### C. Lower-level boot services with larger, simpler grants

The boot modules shrink to a small core whose state rarely changes. They give coarse grants to persistent services, which build the rest inside them.

- **Examples.** A frame service grants pinned ranges of physical memory. A block service grants extents of a partition. The log grants a slice of the open entry. A persistent memory service decides which pages to drop and charges banks, inside its frames. A persistent store keeps regions and the block map inside its extents, and hands out derived caps that it can rescind.
- **The core is what the pager path needs to reach the log and the disk,** and nothing that holds policy. Its state is a few large grants, which are simple to save as data and to restore.
- **A persistent service inside its grants** pages its own memory only in pinned frames, so it never waits on itself. It still needs a way through checkpoint mode, as in option A, but its grants don't move during one.
- **More interfaces cross the line,** and each grant interface becomes part of what a release promises to keep (ADR 0031).

## Decision

To be decided by the investigation that #1001 asks for. For each option, it should answer:

1. **What stays on the pager path,** and what its saved state is.
2. **The restore order:** what has to run before the first fault on a persistent page, and where its code comes from.
3. **Checkpoint mode:** how a service that the checkpoint passes through stays consistent with the snapshot without being write-protected.
4. **Memory:** whether `Boot_Module_Pages` and the build-time table sizes go away, and what replaces them.
5. **Caps:** whether the store can hand out derived caps that survive a restore (#1000, #967), and mem can hold bank caps (ADR 0015).
6. **The rules of #914 and #997:** which processes may then call which while a fault is being served.
7. **Updates and migration:** what a 0.5 update boot replaces, which data formats and interfaces change, and how a 0.4.x checkpoint moves over through the bridge release (#890, ADR 0031).

The investigation should try the chosen direction on one service first. The store is the likely first case, since #1000 has already set out its blockers.

## Consequences

- **#1000 is answered by this ADR,** or folded into it.
- **Until it is accepted,** ADR 0008's split stands, and new pager-path work follows its rules: no cap held across a restore, memory from the reservation, and no call that can wait on a paged process.
