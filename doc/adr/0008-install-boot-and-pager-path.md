# 8. An install boot, and a hard-disk boot with only the pager path

- **Status:** Proposed
- **Date:** 2026-10-27
- **Related:** #321 (partition layout), #322 (booting from hd0), #327 (fresh pages after a restore), #305 (allocating storage on first write), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0005](0005-persistent-console.md) (persistent console)

## Context

Every boot starts the same 15 boot modules from the ISO, whether it installs Rose or restores it. Boot modules are not persistent. A restore brings back the persistent processes as they were at the last checkpoint, but each boot module starts afresh, with only the state it is given again.

That makes a recurring class of bug: something a persistent process depends on isn't part of the checkpoint. It happens when a persistent process holds a cap to a boot module that has state of its own, and that state is neither persistent nor saved. The boot console was one case (ADR 0005): the screen was lost on a restore. Others are still there:

- **`caps`.** `tests` and `petal` hold the cap-set constructor, and nothing saves or restores the cap sets it has made.
- **`timer`.** Persistent processes can set timeouts, and the pending timeouts are not saved.
- **`elf`.** The persistent `exec` launches every program through `elf`.

Each of these could be fixed one at a time, as the console was. But the cause is the same each time: a process that is only needed to install Rose, or that could just as well be persistent, is a boot module.

Installing and restoring are different jobs. An install needs the install media (`isofs`), has to create the partition table (`scan`), and launches everything for the first time. A restore needs none of that. It needs only what it takes to read the checkpoint back, and to handle page faults and further checkpoints from then on.

### The pager path

Not every boot module can become persistent. A checkpoint does four things (`src/servers/checkpoint/src/checkpoint-server.adb`):

1. the kernel takes a snapshot of its tables;
2. `store` appends its region table to the log;
3. `mem` appends its tables and the dirty pages to the log;
4. the kernel leaves checkpoint mode.

Pages go from `mem` to `log`, and on through `partition` and `ata`, so writing a checkpoint uses `store` only for its region table. But a page fault goes from the kernel to `mem`, then to `store`, `partition` and `ata`. If one of those were an ordinary persistent process, two things would go wrong:

- **Restoring it would deadlock.** Its pages would be brought in by a page fault that it has to handle itself.
- **A checkpoint would deadlock.** While a checkpoint is active, the kernel write-protects the pages of persistent processes. A write by the process would fault, and the fault would wait for that same process to handle it.

The processes that a page fault, a checkpoint or a restore passes through are the **pager path**: `mem`, `store`, `partition`, `ata`, `log`, `checkpoint` and `restore`. KeyKOS, EROS and Coyotos have the same constraint. They solve it by putting the object store and the checkpoint in the kernel.

## Decision

### 1. Two kinds of process

- **Pager-path processes are boot modules, and save their state as data.**
  - Each one writes the state that persistent processes depend on to the log at a checkpoint, and is given it back at a restore.
  - The kernel already does this with its tables, and `store` with its region table (`Restore_Region`).
  - Their own memory is never part of a checkpoint.
- **Every other process is persistent.** It is installed once and is restored, never relaunched.

A persistent process may hold caps to pager-path processes. Those caps must work after a restore: each such interface (`Memory`, regions, log append) needs a restore story, as store's regions have. A pager-path process holds no caps to persistent processes.

### 2. An install boot

The ISO boots with the install module set:

- the pager path;
- `isofs` and `scan`;
- the boot console;
- the modules that are installed persistently from then on.

It creates the partition table and installs every persistent program, as a first boot does now. Then it takes a checkpoint and reboots.

### 3. A hard-disk boot

Every later boot is a restore, from hd0 (#322). GRUB loads only:

- `init` and `restore`;
- the pager path: `mem`, `store`, `ata`, `partition`, `log` and `checkpoint`;
- the boot console, for messages before the restore and on the serial port. No persistent process holds a cap to it (ADR 0005).

If there is no checkpoint to restore, the hard-disk boot stops and says to boot the install media.

### 4. What becomes of today's boot modules

| Module | Install boot | Hard-disk boot |
|---|---|---|
| `init`, `restore` | boot module | boot module |
| `mem`, `store`, `ata`, `partition`, `log`, `checkpoint` | boot module | boot module |
| `console` | boot module | boot module, diagnostics only |
| `isofs` | boot module | not loaded |
| `scan` | boot module, creates the partition table | not loaded. `init` or `restore` reads the GPT to find swap and log |
| `elf` | installed, persistent | restored |
| `caps` | installed, persistent | restored |
| `pci` | installed, persistent | restored |
| `timer` | installed, persistent | restored |

### 5. Timers on the pager path

`checkpoint` uses `timer` to schedule checkpoints, and `ata` uses it for timeouts. A persistent `timer` can't serve the pager path. The pager path instead uses the kernel's set-timeout and get-ticks caps directly, which is all `timer` wraps. The persistent `timer` serves everything else, and its pending timeouts are saved with it.

### 6. Rebooting

At the end of an install, `init` asks `checkpoint` for a checkpoint now, rather than waiting for the timer. It waits for the log entry to be complete, then reboots, by a reset through the keyboard controller (port `64`, command `FE`).

Until hd0 can boot (#322), the ISO has a second menu entry with the hard-disk module list. It is used for the reboot, and to test the split.

## Consequences

- **The bug class goes away at the source.** A persistent process can only depend on another persistent process, or on a pager-path process that saves its state as data.
- **Every boot after the install is a restore,** so the restore path runs on every boot. Today a first boot never runs it, and restore bugs show up only on the second boot.
- **The pager path is small, and fixed.** It is the code that must be right for persistence to work at all, and so the natural place for SPARK proofs.
- **Pager-path state must be saved explicitly.** Anything a pager-path process keeps that persistent processes depend on has to go into the checkpoint by hand, as store's fresh-page table must (#327). This is the price of not paging the pager path.
- **`elf`, `caps`, `pci` and `timer` become installed programs,** and need install manifests like the other drivers.
- **A restore no longer depends on the boot order of modules that aren't on the pager path.** Persistent processes find boot modules by object id, and only the pager path is left to find.

## Open questions

- **Persistent object ids for pager-path endpoints.** The ids must be the same on the install boot and every hard-disk boot, although the two boots load different modules. They should not depend on load order.
- **`init` on a hard-disk boot.** `init` does less: no install endpoint, no launching. It may be simpler as a separate program, `init` and `install-init`.
- **Updating the pager path.** A new build changes the boot modules on the boot partition, but not the restored processes. The interfaces between them must stay compatible, or an update needs a fresh install.
- **The fresh-page table** (#327) and any other pager-path state that isn't saved yet must be saved before the hard-disk boot can be trusted.
