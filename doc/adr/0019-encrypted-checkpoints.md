# 19. Encrypting the checkpoint log and the store

- **Status:** Proposed
- **Date:** 2026-10-30
- **Related:** [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0005](0005-persistent-console.md) (persistent console), [ADR 0008](0008-install-boot-and-pager-path.md) (boot modules and the pager path), [ADR 0016](0016-random-numbers.md) (random numbers), [ADR 0018](0018-credentials-after-restore.md) (credentials after a restore), #42 (SPARK), #476 (random numbers)

## Context

Everything that makes a Rose system what it is, is on its disks, and none of it is encrypted:

- **The checkpoint log** holds each persistent process's memory and caps, the kernel's tables, and the console's copy of the screen. A session's credentials are in it, as are the keys of any process that keeps one in memory.
- **The store's regions** hold the installed programs, and the data that persistent processes have written to a region.
- **The boot disk** holds GRUB, the kernel and the boot modules. They are needed to start the system, and aren't secret.

A disk that is stolen, or a machine that is repaired or resold, gives all of that away. ADR 0018 stops someone who sits at a restored machine, and notes that it doesn't help here.

### What shapes the answer

- **There is one path to the disks.** ADR 0008 names the pager path: `mem`, `store`, `partition`, `ata`, `log`, `checkpoint` and `restore`. Pages go from `mem` to `log`, then through `partition` and `ata`. The store reaches its device through `partition` and `ata` too.
- **`log` and `store` use nothing of a device but `Block_Device`.** Each is given its device as a cap (`Add_Log_Device`, `Add_Backing_Store`), opens a `Block_Device` client on it, and calls only `Get_Parameters`, `Read_Blocks` and `Write_Blocks`. Neither names a partition or the ATA driver. A cap to another `Block_Device` can be given in its place.
- **Both do all their I/O in whole pages.** The store requires blocks of 4096 bytes (`Block_Size = Page_Size`) and reads and writes one at a time. The log's device may have blocks of 512 bytes, and the log works out how many it needs for a page (its `Multiplier`, 4096 divided by the block size). Every read and write goes through one procedure, `Operate`, with a start that is a page number times the multiplier and a count that is a whole number of pages, and a page that isn't 4096 bytes is refused before it gets there. Neither reads or writes anything smaller or unaligned.
- **Boot modules aren't persistent.** A checkpoint holds no memory of a pager-path process, and a restore starts each of them afresh with only the state it is given again (ADR 0008). A key held by one of them is never written to the log.
- **A restore needs the key before it can start.** `restore` reads the log to find the last checkpoint. Nothing persistent runs yet, so nothing can ask the user, except through the boot console.
- **A long key typed at every restart isn't acceptable.** Restores follow every reboot.
- **Random numbers are a service** (ADR 0016), with ChaCha20 and BLAKE2s as its primitives. Keys have to come from it, and its first user is the installer.
- **The primitives have to build for both targets,** with no heap and no generics, and are candidates for the SPARK work (#42).

## Decision

### 1. Encrypt blocks, in a boot module on the pager path

A new boot module, `crypt`, sits between the devices and the servers that use them. It implements `Block_Device`, and `log` and `store` are given its caps in place of the partitions'.

- **It encrypts every block that is written and decrypts every block that is read,** with a key that only it holds. The cipher is keyed by the address of the block (see 4).
- **One mechanism covers the log and the store.** A scheme in `log`, and another in `store`, would be two places to get it right, would need two keys, and would be one more format to restore.
- **Nothing above it changes.** The checkpoint format, the region table and the block map are the same, and so are the callers of `Block_Device`.
- **The boot disk isn't encrypted.** GRUB, the kernel and the boot modules have to be read before anything can ask for a key. They hold no secrets, and 5 deals with their integrity.

### 2. The key is never in a checkpoint

`crypt` is a boot module, so it is not persistent and its memory is never part of a checkpoint (ADR 0008). It starts with no key at every boot, and is given one before it serves any I/O. A checkpoint can't contain it, and there is nothing to save or restore.

`restore` does not proceed until `crypt` has a key. If it can't get one, restore stops and says so on the boot console, and the disk stays as it was.

### 3. A data key, and slots that unlock it

- **The disk has one random data key.** The installer makes it with the random number service (ADR 0016), and it encrypts every block. It is never typed.
- **It is stored on the disk wrapped by one or more unlock keys,** in a header at the start of the log's partition that is not encrypted. Each wrapping is a slot, as in LUKS. Adding or removing a way of unlocking changes a slot and doesn't re-encrypt the disk.
- **`crypt` unwraps the data key with whatever slot a key provider can open.**

### 4. Where the unlock key comes from

`crypt` gets an unlock key from a `Key_Provider` cap, tried in this order. The first that works wins, and the order is configured at install.

1. **A TPM or a similar chip,** with the unlock key sealed to the boot chain: GRUB, the kernel and the boot modules. It is released only if they are the ones that sealed it, so nothing is typed, and a disk on another machine is useless. A PIN of a few digits can be added, which the chip rate-limits.
2. **A key on a removable device,** such as a USB stick, that has to be present at boot.
3. **A passphrase,** typed at the boot console. It is always available, and is the way in if the others fail. It needs to be long, so it is the fallback and not the usual way.

The provider that returns a key is a separate module for each kind, so that `crypt` never knows how the key was obtained, and the key is held only by `crypt` and the provider that opened it.

### 5. What is protected, and what isn't

- **Confidentiality of the disks** is the aim of the first version: a stolen disk, a resold machine, or a copy of a disk reveals nothing.
- **Integrity is not.** Block encryption with a tweak by address doesn't detect that a block was changed, or put back from an older copy of the same disk. An attacker who can write the disk could roll the system back. Authenticated encryption needs space in each block for a tag, and a record of freshness, and is left to a later decision.
- **The boot disk's integrity** is covered only if the first provider in 4 measures it, because it is what the TPM seals to.

### 6. The cipher

The cipher is a tweakable one, keyed by the address of the block, and it encrypts in units of 4096 bytes (a page, which is the store's block and the log's group of blocks). `crypt` presents each device with the block size that the underlying one has, so `log` and `store` see no difference. Both already read and write whole, aligned pages, so each request is a whole number of units and `crypt` never has to read and change part of one. It has to build for both targets, have no tables that depend on the key in a way that leaks through timing, be small enough to prove under #42, and be fast enough on a processor without AES instructions.

The first candidates are a ChaCha20-based wide-block mode (as Adiantum is), which shares its primitive with the random number service, and AES-XTS, which needs hardware support to be fast and constant time. This is an open question, and the decision to make before the code.

## Consequences

- **Restore needs a key.** A machine with a TPM restores unattended. One without, and no removable device, asks for the passphrase at every restore. That is a cost that can only be removed by hardware.
- **The key is in `crypt`'s memory while the system runs.** A process that could read its memory could read the key, so `crypt` holds no caps that others need, and no one is given a cap to it except the processes that serve the devices. Memory that is read from outside the system, as in a cold boot attack, is not covered.
- **Everything that is paged out is covered,** because `mem` writes pages through the log. So is the console's screen copy (ADR 0005), which is in a checkpoint.
- **I/O is slower.** Every block goes through `crypt`, which is a hop and a cipher. A boot module on the pager path has to be written without page faults of its own (ADR 0008), and its tables are written when it starts, as the store's now are.
- **A disk needs a header.** It holds the slots, and is the one place on the log's partition that isn't encrypted. A damaged header makes the disk unreadable, so a copy is needed.
- **The install image is unencrypted.** It holds no user data, and the installer sets up the encrypted disks.
- **Per-user data isn't covered by this.** Anyone who unlocks the machine reads every user's data. A key per user, from the login password, is a different decision (see Open questions).

## Alternatives considered

- **Encrypt the checkpoint log only.** It misses the store, which holds the installed programs and what persistent processes wrote to a region. It would also put a second scheme in `store` later.
- **Encrypt in `log` and in `store` separately.** Two keys, two schemes and two places to be wrong, for what a block device layer gives once.
- **Encrypt in the kernel.** The kernel has mechanism and not policy, and a key in the kernel is one more thing that a checkpoint has to treat specially. A boot module on the pager path is outside every checkpoint already.
- **A key typed at every restart.** It is the one way that works everywhere, so it is kept as a fallback, but it is rejected as the way in, since restores follow every reboot.
- **A key stored on the boot disk.** It protects a disk that is stolen without its boot disk, and nothing otherwise, since the boot disk is read to start the system.
- **Self-encrypting drives** (such as the TCG Opal ones). They need the drive's support and trust in its firmware, and don't help a copy made by another route. They could be used as well.
- **Encrypting each process's memory in `mem`.** `mem` writes pages to the log, which is already covered in one place. It would add a key and a policy to a server that has neither.

## Open questions

- **The cipher,** as in 6, and what to do about authentication.
- **Freshness.** Whether a rollback of the disk to an older state needs detecting, for example with a counter in the TPM, and what a rollback costs a persistent system (the whole machine goes back, as a restore from a checkpoint does).
- **TPM support.** What drivers it needs, whether QEMU's emulated TPM is enough to develop against, and what has to be measured for a seal to mean something.
- **Where the header lives,** and how a copy is kept. `crypt` can hide it by presenting a device that is smaller than the one underneath, so `log` and `store` never see it. With two encrypted partitions, whether they share one data key and one header, or each has its own, is not decided.
- **Recovery.** A recovery key shown at install, and what to do if the passphrase and the other slots are lost.
- **Per-user keys.** Whether a user's own data, perhaps their home directory, is encrypted under a key from their login password, so that unlocking a session (ADR 0018) also unlocks that user's data, and one user can't read another's even with the machine unlocked. It makes a forgotten password a data loss.
- **A key provider as a cap.** How `crypt` is given the cap for each provider, and by whom, since it has to be before `restore` runs.
- **Changing the data key.** Whether it can be replaced without re-encrypting every block.

## Follow-up work

- A `Block_Device` wrapper, `crypt`, on the pager path, and the install and restore changes that go with it (new issue).
- The key header and slots, and key providers for a passphrase, a removable device and a TPM (new issues).
- The cipher, with test vectors and a proof plan under #42 (new issue).
