# 31. Updating an installation

- **Status:** Proposed
- **Date:** 2026-11-09
- **Related:** #738 (this decision), #375 (checkpoint format version and boot server updates), #737 (restore test from the 0.4.0 image), #736 (endpoint ids), #460 (the command registry), #413 (the install media boots hd0), #824 (replacing a running persistent process), #825 (cap sets), #829 (installing the commands again), [ADR 0008](0008-install-boot-and-pager-path.md) (install boot and pager path, whose open question on updating the pager path this answers), [ADR 0014](0014-banks.md) (banks), [ADR 0015](0015-process-ownership.md) (process ownership), [ADR 0021](0021-privilege.md) (privilege), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), [ADR 0029](0029-installed-capabilities.md) (installed capabilities), #890 (updating across a release series)

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

For the level to mean anything, `endpoint-ids.txt` is **append-only within a series**. After `v0.4.0` is tagged, `make check-endpoint-ids` also compares the file with the one at the series' first tag, and fails if any line of it has changed or gone. A 0.4.x can add interfaces and operations, and can't change or remove one. Changing one waits for 0.5.0, which starts a new series (section 7).

**An interface or operation that is no longer wanted is retired, not removed.** A restored process may still call it, so it can't go away within the series:

- **Servers go on answering it,** as they did. A server can answer it through the operation that replaces it, but a call that worked in 0.4.0 still works.
- **New code avoids it.** A new server doesn't implement a retired interface unless a client from the series needs it, and a new client doesn't call it.
- **The IDL marks it,** with a comment on the line before the operation or interface: `--  Retired in 0.4.N (#issue): use X instead.` Its line stays in `endpoint-ids.txt`.
- **Removing it waits for the next series,** and then only if no process carried over from this series calls it (section 7).

This isn't the retired program image of section 4, which can't be launched any more. A retired operation still works.

A refused restore prints both sides, such as `checkpoint: interfaces 0.4 level 213; this boot set: 0.5 level 190`, and the boot stops as when there is no checkpoint.

This is #375's point 1 and point 2. Section 7 says how a new series takes over a checkpoint from the one before.

### 3. Two boot sets on the boot partition

The boot partition holds two boot sets, in `/boot/a` and `/boot/b`, and `/boot/grub/grub.cfg` loads the current one by default.

- **Writing.** The update boot writes the new set into the slot that isn't current, writes `grub.cfg` again, and only then makes the new slot current. A failure before the menu is written leaves the old set in charge.
- **The menu.** It has the current set's entries, as now, and a **Previous: Rose X.Y.Z** entry for the other slot. If the new set boots but something goes wrong later, the previous one still restores, because the checkpoint it last wrote has an interface level that it accepts. A checkpoint written by the new set may not be accepted by the old one if the new set added interfaces. Then the previous entry refuses, and says so.
- **Partition servers.** The update boot uses the boot partition's partition server (object id 20), which today only the install boot launches.

### 4. Updating installed programs

The update boot installs every program on the install media again, as the install boot does, with one difference: the programs that are restored are left running.

- **Commands.** For each command, the updater calls `Exec.Update` with the new image and the command's name, and replaces the registry's entry with `Map.Add`, which replaces a name that is already there. Petal finds the new launch cap at the next `Commands.Find`, so the next run of the command has the new code. A command that is running finishes with its old code.
- **Unchanged programs.** `Exec.Update` compares the new image, page for page, with the installed program of that name. If they are the same, it gives back the pages it wrote and returns the installed program's launch cap, so an update that changes nothing doesn't grow `exec`'s region. The comparison is by name, in `exec`, because a server can't tell which of its own objects a cap passed to it names.
- **Installed capabilities.** A command with installed arguments (ADR 0029) is given them again, resolved from its install manifest as the installer does. A cap's number says nothing about what it names, so a program with install caps always counts as changed.
- **The old image is retired, not freed.** A program's read-only segments, its code included, are mapped from `exec`'s region and shared by every process running it, and `exec` doesn't know which processes those are. So `Exec.Update` retires the old version: launching it fails from then on, and its pages stay. Freeing them waits until banks can show that nothing maps them (ADR 0014).
- **`file` and `directory` are left as they are.** Every directory process holds their launch caps, to create what is in it, and retiring the old versions would stop it. Their new code comes when they are replaced (section 5, ADR 0032).
- **Where it runs.** The updater is `restore-install`'s installer, which has the install media. It runs on the update boot only, once the restored processes are running, and isn't a persistent process. Init waits for it before taking the checkpoint that it restarts into.
- **How it reaches `exec` and the registry.** Nothing on the update boot holds a cap to them: init held `Exec.Install` and `Map.Add` on the install boot, and exits. So the kernel gives restore-install, and nothing else, a cap that makes an endpoint cap to a restored process for an endpoint it has registered. The updater finds `exec` and the registry by the interface endpoints they answer, `Exec` and `Map`, which are unique (section 2), with identifier 0, which names a server's first object. An `exec` installed before `Exec.Update` existed has no cap for it, and then the commands are left as they are.

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

### 7. Updating to a new series

A new series updates a disk from one release of the series before it, its **bridge release**, and from no other. 0.5.0 updates a disk that 0.4.N wrote. A disk on an earlier 0.4.x is updated to 0.4.N first, with 0.4.N's install media, and then to 0.5.0. A disk on a later 0.4.x is refused.

N is chosen when 0.5 starts, and is normally the last 0.4.x. The release notes of 0.5.0 name it.

The update boot is the one of section 1. A new series doesn't need a separate mechanism, because it keeps its promises to the series before it in the same way as a release within a series, but only to one release.

- **Table formats.** 0.5.0's `restore` reads 0.4.N's tables as well as its own, and converts them as it loads them. The checkpoint the update boot takes (section 1, step 5) is in 0.5's formats, so a later 0.5.y needn't read 0.4's. A change that can't be converted as it loads, such as a new log layout on disk, is converted by the update boot before the restore. That is still a conversion from one known format.
- **Interfaces.** 0.5.0 accepts a log entry of series 0.4 only at 0.4.N's interface level. Any other entry is refused, and the message says what to do: `checkpoint: interfaces 0.4 level 198; this boot set updates from 0.4 level 213 (0.4.7): update to 0.4.7 first`.
- **Endpoint ids.** The persistent processes that the update boot doesn't replace (section 5) still call through 0.4.N's interfaces. So 0.5.0 keeps every endpoint id of 0.4.N that one of them can call, including the retired ones (section 2), and answers it as 0.4.N did. A rename, such as ADR 0034's, pins the old id. The other ids of 0.4.N may change or go. `check-endpoint-ids` compares 0.5's list with 0.4.N's as well as with 0.5.0's, and the ids that 0.5 drops are listed in the tree, each with the reason it is safe.
- **Replaceable servers.** The update boot replaces them as in section 5. The new version reads the state its 0.4.N predecessor saved (ADR 0032, section 2), so each such server's reader for 0.4.N's format stays in 0.5.0. Later 0.5.y releases read only what 0.5.0 saved.
- **Processes that can't be replaced.** They are what keeps the 0.4.N interfaces in 0.5. A 0.4.x before N can shrink that set by making `exec` or the registry replaceable. That is an addition, so a 0.4.x can make it.

Nothing in this section is needed in 0.4.0 beyond what sections 2 and 5 already require: the interface level in each log entry, and `Replaceable` and `Saveable` in the servers that hold state. Anything else 0.5 needs from 0.4 can be added by a 0.4.x up to N.

**Testing.** The tests of section 6 run for 0.5.0 against 0.4.N's image as well. `scripts/make-restore-fixture` makes an image from 0.4.N, which is uploaded to the 0.4.N release, and 0.5.0's update boot is run on it.

## Consequences

- **A 0.4.x release can fix the kernel, the boot modules, the commands and the replaceable servers** without losing the checkpoint.
- **It can't fix a persistent process that isn't replaceable.** A bug in `exec`, the registry, or a driver that can't hand over its device stays until a new install.
- **0.4.0 needs #824 and #825 before it is tagged.** `Replaceable` and the changes to `Cap_Set` are interfaces, and the interfaces freeze with the tag.
- **The interfaces of 0.4.0 are frozen for the whole series.** Additions are allowed, and so is retiring an interface or operation (section 2). `check-endpoint-ids` enforces the rest.
- **Updating needs the install media,** as installing does. A network update can use the same steps once there is a network (ADR 0007), with the media replaced by a downloaded image.
- **The boot partition holds two boot sets,** about 7 MB of its 16 MiB. A much larger boot set needs a larger boot partition, which needs a new install.
- **An update boot is a restore,** so it runs the restore path with a new boot set, which is what #737 tests.
- **Every series can take over the one before it,** from one release, so an installation can be updated indefinitely, through each bridge release in turn.
- **A new series carries a little of the old one** in its first release: readers for the bridge release's table and state formats, and the endpoint ids that its unreplaced processes call. A later release of the new series drops the readers.
- **0.4.0 commits to nothing new for 0.5.** 0.5's needs are met by 0.4.N, which can still add to the 0.4 interfaces.

## Alternatives considered

- **Write the new boot set without restoring first.** Simpler, but a set that can't restore the checkpoint reaches the disk, and the user finds out at the next boot. The two slots would recover from that, but there would be no message saying why.
- **An `update` command in the running system,** run with `sudo` after logging in, reading the install media through `isofs`. It would need a persistent process to hold caps to the install media's boot modules, which ADR 0008 avoids, and a privileged role to replace the boot partition. The update boot already has both, and nothing else is running.
- **Commands as files, loaded at each launch.** Updating a command would then be replacing a file in a transaction (ADR 0026). But `exec` copies images into its region for good reasons, a program can't be changed under a running process and the launch cap is the program, and changing that is a larger decision than updating.
- **Promise only that old code runs,** with no way to update. Every 0.4.x would then be a fresh install, and the stable-restore promise of 0.4.0 would have no use.
- **Leave persistent processes to a later series,** updating only the boot set and the commands in 0.4.x. It is less work for 0.4.0, but a bug in `file` or `directory` would need a fresh install for the whole series, and the servers added later, such as networking and the display, would have to retrofit a handover rather than be built with one.
- **One boot set, overwritten in place.** It saves 3.6 MB, and loses the way back if a new set fails after the restore has succeeded.
- **Export and import across a series.** Every persistent server writes its saved state to a file or a partition, and a fresh 0.5 install reads it, rebuilding the caps between servers by name. It needs every persistent server of 0.4.0 to be `Saveable`, decided before the tag, and a second way of rebuilding a system. Handing over during an update boot already reads a predecessor's saved state, and keeps the caps.
- **A converter for every 0.4.x.** A 0.5 tool that rewrites any 0.4 checkpoint: kernel tables, `mem`'s records and endpoint ids. It couples 0.5 to every 0.4 format. Converting from the bridge release only is the same work, done once.
- **A fresh install for each series.** The cheapest, but each series would then lose every installation of the one before, and the stable-restore promise of 0.4 would end with 0.4.
