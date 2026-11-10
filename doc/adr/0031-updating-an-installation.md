# 31. Updating an installation

- **Status:** Proposed
- **Date:** 2026-11-09
- **Related:** #738 (this decision), #375 (checkpoint format version and boot server updates), #737 (restore test from the 0.4.0 image), #736 (endpoint ids), #460 (the command registry), #413 (the install media boots hd0), #824 (replacing a running persistent process), #825 (cap sets), [ADR 0008](0008-install-boot-and-pager-path.md) (install boot and pager path, whose open question on updating the pager path this answers), [ADR 0014](0014-banks.md) (banks), [ADR 0015](0015-process-ownership.md) (process ownership), [ADR 0021](0021-privilege.md) (privilege), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), [ADR 0029](0029-installed-capabilities.md) (installed capabilities)

## Context

The only way to get a new release of Rose onto a disk today is to install it again, which loses the checkpoint: every file, user and session. 0.4.0 promises that a checkpoint made by 0.4.0 restores on every later 0.4.x, so 0.4.x needs a way to put a new build on a disk that keeps its checkpoint.

An installed system has three layers, and a new build reaches each one differently.

1. **The boot set.** GRUB loads the kernel and the boot modules from the boot partition, `rose-boot`, on every boot (`boot/hd0/grub.cfg`). None of them is saved in a checkpoint, so a new boot set takes effect at the next boot. It has to read the old checkpoint, though.
   - **Table formats.** The kernel's, `mem`'s and `store`'s tables in a log entry each have a version, and so does the log header (`Log_Version`). `restore` already refuses an entry whose table version it doesn't know (`Restore.Checkpoint_Log`, "unknown kernel tables version").
   - **Endpoint ids.** Persistent processes hold caps to boot modules, and a cap's endpoint id comes from the IDL (#736). A boot module that changes an interface gives the persistent processes that call it caps that no longer match. Nothing checks this at restore time.
   - **How it gets there.** The boot partition is written only by an install (`restore-install` writes GRUB and the boot set to hd0, #322). Nothing writes it on a disk that has a checkpoint.
2. **Installed programs.** `Exec.Install` copies a program's image into a region that `exec` owns, and returns a launch cap (`Exec.Library.Install`). The installer adds each command's launch cap to the command registry (`Map.Add`, #460), where petal finds it (`Commands.Find`). So a program's code is in the checkpoint, in `exec`'s region, and not in a file. A command gets fresh code only if it is installed again, and the registry is given the new launch cap.
3. **Persistent processes.** `exec`, `file`, `directory`, the drivers, `auth`, the user server and every running session were launched once and are restored, never relaunched (ADR 0008). Both their code and their state are in the checkpoint. A new build doesn't reach them at all.

The boot partition is a 16 MiB FAT file system. The boot set, the kernel and 14 modules, takes about 3.6 MB, and 8.9 MB is free, so a second boot set fits.

There is no network, so a new build arrives on its install media.

## Decision

### 1. An update boot, from the install media

The install media gets a third GRUB entry, **Update Rose**, next to its default entry, which boots hd0's own menu (#413), and **Install Rose**. It loads the install media's own kernel and boot modules: the *new* boot set, in its install variant, with `isofs`. Then:

1. **It restores hd0's checkpoint with the new boot set.** If the checkpoint isn't compatible (section 2), `restore` refuses, says why, and stops. Nothing has been written to hd0, and booting without the install media boots the old set as before.
2. **It writes the new boot set to hd0** (section 3), only after the restore has succeeded.
3. **It updates the installed programs** (section 4).
4. **It replaces the persistent processes that can be replaced** (section 5).
5. **It takes a checkpoint and restarts from hd0,** as an install does.

The update boot is the only thing that writes to an installed disk's boot partition, as the install boot is the only thing that writes to an empty disk.

Restoring with the new set before writing it tests the new set against this disk's own checkpoint, so a set that would refuse the checkpoint never reaches the disk.

### 2. What a checkpoint must match

A boot set can restore a checkpoint if two things match.

- **Table formats.** The table versions in the log entry, as now. A new table format in 0.4.x needs `restore` to read the old one as well, or the release can't be a 0.4.x.
- **Interfaces.** Each log entry records an **interface level**: the release series, such as 0.4, and the number of lines in `interfaces/endpoint-ids.txt` when the boot set was built. A boot set restores an entry of the same series whose level is no higher than its own.

For the level to mean anything, `endpoint-ids.txt` is **append-only within a series**. After `v0.4.0` is tagged, `make check-endpoint-ids` also compares the file with the one at the series' first tag, and fails if any line of it has changed or gone. A 0.4.x can add interfaces and operations, and can't change or remove one. Changing one waits for 0.5.0, which starts a new series and doesn't restore 0.4 checkpoints.

A refused restore prints both sides, such as `checkpoint: interfaces 0.4 level 213; this boot set: 0.5 level 190`, and the boot stops as when there is no checkpoint.

This is #375's point 1 and point 2. Converting old formats, so that 0.5 could restore 0.4, is left for later.

### 3. Two boot sets on the boot partition

The boot partition holds two boot sets, in `/boot/a` and `/boot/b`, and `/boot/grub/grub.cfg` loads the current one by default.

- **Writing.** The update boot writes the new set into the slot that isn't current, writes `grub.cfg` again, and only then makes the new slot current. A failure before the menu is written leaves the old set in charge.
- **The menu.** It has the current set's entries, as now, and a **Previous: Rose X.Y.Z** entry for the other slot. If the new set boots but something goes wrong later, the previous one still restores, because the checkpoint it last wrote has an interface level that it accepts. A checkpoint written by the new set may not be accepted by the old one if the new set added interfaces. Then the previous entry refuses, and says so.
- **Partition servers.** The update boot uses the boot partition's partition server (object id 20), which today only the install boot launches.

### 4. Updating installed programs

The update boot installs every program on the install media again, as the install boot does, with one difference: the programs that are restored are left running.

- **Commands.** For each command, the updater calls `Exec.Install` with the new image, and replaces the registry's entry with `Map.Add`, which replaces a name that is already there. Petal finds the new launch cap at the next `Commands.Find`, so the next run of the command has the new code. A command that is running finishes with its old code.
- **Installed capabilities.** A command with installed arguments (ADR 0029) is given them again, resolved from its install manifest as the installer does.
- **The old image.** `exec` keeps the old image's region until nothing holds its launch cap. Until banks can tell `exec` that, it needs an `Exec.Uninstall`, which the updater calls for the old launch cap after `Map.Add`. A process still running the old image keeps its own pages.
- **Unchanged programs** are skipped: the updater compares the image's size and contents with the installed one before installing it.
- **Where it runs.** The updater is `restore-install`'s installer, which already has the code, the registry and `exec`. It runs on the update boot only, and isn't a persistent process.

### 5. Replacing persistent processes

A persistent process is replaced with its new version only if its server implements `Replaceable` (#824), which is part of 0.4.0. Otherwise it keeps the code it was installed with.

- **Replaceable.** The old version exports its state, with the caps it holds in a cap set (#825), and the new version reads it at launch. The kernel then moves the old process's object ids to the new one, so clients keep their caps, and the old process ends. If the new version refuses the state, it ends, and the old one carries on. #824 has the design, and its own ADR decides the details.
- **Which ones.** The long-lived servers whose state can be handed over, such as `file`, `directory`, `auth` and the user server, and drivers whose devices can be.
- **Which ones not.**
  - `exec`, which launches the new versions.
  - The command registry, which the updater uses.
  - Drivers whose devices can't be handed over safely.
  - Running sessions, which are simpler to end and start again: the next login gets the new petal (section 4).
- **The order.** The updater replaces servers one at a time, each after the programs are installed again, so that a new version can rely on the new commands. A server that fails to take over leaves the old one running, and the updater reports it and goes on.

The release notes of each 0.4.x say which fixes are in servers that aren't replaceable, and so need a new install to take effect.

The promise of section 2 still holds. A replaced server answers the same endpoint ids as the one it replaces, because a new version within a series only adds to the interfaces. The persistent processes of a 0.4.0 checkpoint call the boot modules and the replaced servers through the 0.4.0 interfaces, and every 0.4.x still answers them.

### 6. Testing

The restore test from the 0.4.0 image (#737) checks section 2 on every build. The image is a disk that 0.4.0 installed and used, made by `scripts/make-restore-fixture` and uploaded to the 0.4.0 release; `scripts/test-restore-fixture` writes the build's boot set onto a copy, restores it, and runs the self tests. A second test runs an update boot on the 0.4.0 image with the current install media, and checks:

- that hd0 then boots the new set;
- that a command reports the new version;
- that a replaced server, such as `file`, reports the new version, and the files opened before the update still work;
- that **Previous** still boots 0.4.0.

## Consequences

- **A 0.4.x release can fix the kernel, the boot modules, the commands and the replaceable servers** without losing the checkpoint.
- **It can't fix a persistent process that isn't replaceable.** A bug in `exec`, the registry, or a driver that can't hand over its device stays until a new install.
- **0.4.0 needs #824 and #825 before it is tagged.** `Replaceable` and the changes to `Cap_Set` are interfaces, and the interfaces freeze with the tag.
- **The interfaces of 0.4.0 are frozen for the whole series.** Additions are allowed, and `check-endpoint-ids` enforces the rest.
- **Updating needs the install media,** as installing does. A network update can use the same steps once there is a network (ADR 0007), with the media replaced by a downloaded image.
- **The boot partition holds two boot sets,** about 7 MB of its 16 MiB. A much larger boot set needs a larger boot partition, which needs a new install.
- **An update boot is a restore,** so it runs the restore path with a new boot set, which is what #737 tests.

## Alternatives considered

- **Write the new boot set without restoring first.** Simpler, but a set that can't restore the checkpoint reaches the disk, and the user finds out at the next boot. The two slots would recover from that, but there would be no message saying why.
- **An `update` command in the running system,** run with `sudo` after logging in, reading the install media through `isofs`. It would need a persistent process to hold caps to the install media's boot modules, which ADR 0008 avoids, and a privileged role to replace the boot partition. The update boot already has both, and nothing else is running.
- **Commands as files, loaded at each launch.** Updating a command would then be replacing a file in a transaction (ADR 0026). But `exec` copies images into its region for good reasons, a program can't be changed under a running process and the launch cap is the program, and changing that is a larger decision than updating.
- **Promise only that old code runs,** with no way to update. Every 0.4.x would then be a fresh install, and the stable-restore promise of 0.4.0 would have no use.
- **Leave persistent processes to a later series,** updating only the boot set and the commands in 0.4.x. It is less work for 0.4.0, but a bug in `file` or `directory` would need a fresh install for the whole series, and the servers added later, such as networking and the display, would have to retrofit a handover rather than be built with one.
- **One boot set, overwritten in place.** It saves 3.6 MB, and loses the way back if a new set fails after the restore has succeeded.
