#!/usr/bin/env python3
"""Write a boot set onto an installed disk image (#737).

    write-boot-set.py DISK [BOOT_DIR]

BOOT_DIR is what scripts/make-boot-disk builds, build/install/boot by
default: boot.img, core.img and boot.fat. They go where the installer
puts them (Restore.Boot_Disk): the first 440 bytes of boot.img to block
0, keeping the protective MBR's partition entry and signature; core.img
to the BIOS boot partition; and boot.fat to the boot partition, the
second in the GPT, which GRUB's core.img names as (hd0,gpt2).

The partitions are read from the disk's GPT, so the disk keeps its own
layout. core.img holds the block where it starts, which make-boot-disk
sets, so it is refused if the disk's BIOS boot partition starts
elsewhere. Anything that doesn't fit is refused too, before anything is
written.
"""

import os
import struct
import sys
import uuid

BLOCK = 512
MBR_CODE = 440
BIOS_BOOT_TYPE = uuid.UUID("21686148-6449-6E6F-744E-656564454649")


def fail(message):
    sys.exit("write-boot-set: " + message)


def partitions(disk):
    """The GPT's entries, as (index, type, first block, last block)."""
    disk.seek(BLOCK)
    header = disk.read(92)
    if header[:8] != b"EFI PART":
        fail("no GPT header at block 1")
    entries_lba, count, size = struct.unpack_from("<QII", header, 72)
    disk.seek(entries_lba * BLOCK)
    table = disk.read(count * size)
    result = []
    for i in range(count):
        entry = table[i * size:(i + 1) * size]
        kind = uuid.UUID(bytes_le=entry[:16])
        if kind.int == 0:
            continue
        first, last = struct.unpack_from("<QQ", entry, 32)
        result.append((i + 1, kind, first, last))
    return result


def main():
    if len(sys.argv) not in (2, 3):
        sys.exit(__doc__)
    path = sys.argv[1]
    boot_dir = sys.argv[2] if len(sys.argv) == 3 else "build/install/boot"

    def read(name):
        with open(os.path.join(boot_dir, name), "rb") as f:
            return f.read()

    boot_img = read("boot.img")
    core_img = read("core.img")
    boot_fat = read("boot.fat")
    if len(boot_img) < MBR_CODE:
        fail("boot.img is too short")

    with open(path, "r+b") as disk:
        table = {index: (kind, first, last)
                 for index, kind, first, last in partitions(disk)}
        if 1 not in table or table[1][0] != BIOS_BOOT_TYPE:
            fail("partition 1 is not a BIOS boot partition")
        if 2 not in table:
            fail("no boot partition (partition 2)")
        _, bios_first, bios_last = table[1]
        _, boot_first, boot_last = table[2]

        # core.img's first sector loads the rest from the block list at
        # 0x1f4, which make-boot-disk points at the block after the start
        # of the BIOS boot partition.
        core_from = struct.unpack_from("<Q", core_img, 0x1F4)[0]
        if core_from != bios_first + 1:
            fail("core.img loads from block %d, but this disk's BIOS boot "
                 "partition starts at block %d" % (core_from, bios_first))
        if len(core_img) > (bios_last - bios_first + 1) * BLOCK:
            fail("core.img doesn't fit the BIOS boot partition")
        if len(boot_fat) > (boot_last - boot_first + 1) * BLOCK:
            fail("boot.fat doesn't fit the boot partition")

        disk.seek(0)
        disk.write(boot_img[:MBR_CODE])
        disk.seek(bios_first * BLOCK)
        disk.write(core_img)
        disk.seek(boot_first * BLOCK)
        disk.write(boot_fat)

    print("write-boot-set: wrote boot.img, core.img at block %d, and "
          "boot.fat at block %d" % (bios_first, boot_first))


if __name__ == "__main__":
    main()
