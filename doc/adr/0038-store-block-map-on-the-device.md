# 38. The store's block map on the device, as a copy-on-write tree

- **Status:** Proposed
- **Date:** 2026-11-15
- **Related:** #504 (this decision), #305 (blocks on first write, PR #501), #505 (flushing the disk's write cache), #935 (a stable storage format for 0.4.0), [ADR 0008](0008-install-boot-and-pager-path.md) (every hard-disk boot is a restore), [ADR 0014](0014-banks.md) (banks), [ADR 0025](0025-shared-regions.md) (shared pages written back), [ADR 0028](0028-paging-to-the-log.md) (paging to the log)

## Context

Since #305 the store gives a region's pages space on a backing store when they are first written. A map in the store's memory says where each written page is.

### What exists today

- **The map is a hash table in `store-devices.adb`** with 16384 entries. Each entry is a block of 4 pages: the block's index from the allocator, and a `Fresh` mask of the pages not yet written. That is 256 MiB of written data, and a system can't write more.
- **A checkpoint writes the whole map to the log,** however little has changed. A restore reads it back and gives each block its space again with `Allocate_At`.
- **Space comes from `Rose.Allocators`,** a buddy allocator in the store's memory that addresses 4 GiB. Since #305 it is only ever asked for one block at a time.
- **`Put` overwrites a block in place.** A restore to the last checkpoint can therefore read a page newer than the checkpoint. That is safe only because everything written through `Put` is written once (program images) or also has a copy in the log (shared pages written back, ADR 0025).
- **A block freed after a checkpoint can be given out again** before the next one is committed. A restore to the last checkpoint then finds another region's pages in it.
- **`Release` scans the whole map** for the region's blocks.
- **The store is never told that a checkpoint committed.** The checkpoint server calls the store's `Checkpoint`, then `mem`'s, then leaves the checkpoint, and nothing reaches the store afterwards.
- **The store's memory must never fault.** A fault in the store while `mem` waits on it would deadlock both, so its tables are written when it starts, and every page of them is mapped before it is used.

The map's format is part of what 0.4.0 promises to restore on every later 0.4.x (#935), so it has to be settled before the tag. 0.3 does not update to 0.4, so nothing written by 0.3 has to be read.

## Decision

### 1. The map is a radix tree of pages on the backing store

- **A page of the tree is a page of 512 eight-byte entries.** An entry is a present bit and a device page. An interior entry names a page of the next level down, and a leaf entry names the page that holds a store page's contents.
- **The tree is keyed by the page id less `Page_Object_Id'First`,** 9 bits to a level, so a leaf covers 2 MiB of page ids.
- **Its height grows with `Next_Page`.** A new root is added above the old one when a region's pages need more levels. Page ids are never used again, so a released region leaves an empty subtree, which the tree drops.
- **The tree's pages are blocks of the backing stores,** from the same allocator as the data pages. There is no separate partition, because a partition would have a fixed size, as the table has now.

### 2. A page is the unit of allocation

The store allocates a page at a time, not a block of 4.

- **A page with no entry reads as zeroes,** as an unwritten page does now. The `Fresh` mask goes away.
- **A `Put` of zeroes** to a page that has no space is dropped, as now. To a page that has space, it frees the space.
- **A bank is charged a page for each page that has space.** The tree's own pages are the store's overhead, and no bank is charged for them.

A block of 4 pages kept the map small. With the map on the device its size no longer matters, and section 3 would have to read 3 pages to write 1.

### 3. Data pages are copy-on-write as well as the tree

A checkpoint's generation is the time from one checkpoint to the next.

- **A page whose space was allocated in this generation is written in place.**
- **Any other page is written to new space:** a data page by `Put`, and a tree page whose entry changed. Its parent's entry changes too, and so on up to the root.
- **The old space is freed,** under the rule in section 5.

So a checkpoint's root names its tree and its data as they were at the checkpoint, and nothing written later changes either of them. A restore can't read a page newer than its checkpoint. Paging shared pages out through the store, which ADR 0028 left open, becomes possible.

A program image is written once, and shared pages are written back rarely, so few pages are copied.

### 4. The free map is two fixed bitmaps on each backing store

- **The first pages of each backing store hold two copies of its free map,** a bit a page: a page of bitmap for each 128 MiB of device. They are allocated when the backing store is added, and never given out.
- **Checkpoints write the copies in turn,** by the parity of the generation. A checkpoint writes only the bitmap pages that changed since that copy was last written, two generations before.
- **A restore reads the copy that its checkpoint names,** and the root of the tree. It doesn't walk the tree to find the space in use. On a hard-disk boot, which is always a restore (ADR 0008), the time a restore takes doesn't grow with how much has been written.

A bitmap at a fixed place takes no space from itself, so writing it can't change it. A copy-on-write tree for the free map would need space for its new pages, and so change itself while it is being written.

The bitmap replaces `Rose.Allocators` in the store, which only ever allocated one block. The store hands out space from it by searching from where it last allocated, so that a region's pages tend to be close together.

### 5. Freed space waits for the next commit

- **Space freed in generation N is held** until checkpoint N is committed. Until then the last committed checkpoint may still name it.
- **The store keeps two bitmaps in memory:** the pages allocated in this generation (section 3), and the pages held. Each is a bit a page, so 128 KiB for the 4 GiB that the store addresses now.
- **When a checkpoint is not committed,** the pages held for it go on being held until the next one is.

### 6. The store is told when a checkpoint is committed

`Checkpoints.Storage_Checkpoint` gets a procedure, `Committed`. The checkpoint server calls it after `Leave_Checkpoint`, once the log entry is committed. The store then frees the pages it held for that generation.

### 7. A cache of the tree's pages

- **The cache is a fixed array of pages,** about 64 of them (256 KiB), mapped when the store starts, as its tables are now.
- **It always holds the root,** and the path from the root to every page it holds, so that a change can be carried up to the root without reading the device.
- **A checkpoint writes only the pages that changed,** bottom up, with the root last.
- **When the cache is full of changed pages,** the store writes changed leaves early, to the space that section 3 gives them. The last committed root doesn't name that space, so this is safe. A full cache costs writes, but nothing waits for it.

The hash table and its buckets go, which frees about as much memory as the cache and the bitmaps of section 5 take. The store's memory stays about the same, but it no longer limits how much can be written.

### 8. What a checkpoint writes, and in what order

1. The changed pages of the tree, then the root.
2. The changed pages of this generation's free map copy.
3. When #505 has added `Block_Device.Flush`, a flush of each backing store.
4. The store's table, appended to the log.

The table names the root, so steps 1 and 2 must be on the medium before the entry is committed. Each write returns when it has completed. Until #505, the drive's write cache is the only gap, as it is for the log.

### 9. Releasing a region

`Release` walks the tree under the region's pages, visiting only the entries that are present, and frees each page, and each tree page that it leaves empty, under section 5. The cost grows with how much of the region was written, not with its size. If that proves too slow for a large region, the walk can be done a part at a time between requests.

### 10. The table format

`Store_Table_Version` becomes 5.

- **The header** holds the root's device page, the tree's height, the generation, and which free map copy was written.
- **The map pages go.** The region and bank pages stay. `Saved_Region.Blocks` counts the pages that have space, and a restore charges the banks from it.
- **The restore server** reads only the header's `Page_Count`, and doesn't change.
- **Version 4 is refused,** with a message. 0.3 doesn't update to 0.4.

### 11. What stays as it is

- **The 4 GiB that the store can address.** It becomes a constant, which a later change can raise, along with the size of the bitmaps in section 5.
- **ADR 0025's shared pages,** whose truth stays in the log. Section 3 makes moving them into the store possible, but this decision doesn't do it.

## Consequences

- **What a system can write is limited by its backing stores,** not by the store's memory.
- **A checkpoint writes what changed:** a path of tree pages for each changed leaf, and the changed bitmap pages. It no longer writes the whole map.
- **A restore reads a fixed amount** from the store's part of the log, and then a root page and a bitmap copy.
- **A restore to the last checkpoint is consistent,** for data as well as the map. Space freed after it can't have been given out again.
- **Reading a page that isn't in the cache costs reads of tree pages,** up to the tree's height, before the page itself.
- **A write after a checkpoint allocates,** and the space it replaces is held until the next commit. A store close to full can run out sooner than it does now.
- **`Storage_Checkpoint` gets an operation,** so the interface level goes up.
- **The backing store's layout changes:** the free map copies come first. This has to land before 0.4.0 is tagged (#935).

## Alternatives considered

- **Keeping the map in memory, larger.** That is what exists now. The limit moves but stays, and a checkpoint still writes all of it.
- **A partition for the map.** Its size would be fixed, as the table's is now.
- **Copy-on-write for the tree only.** Simpler, but a restore could still read a data page newer than its checkpoint, and the store couldn't hold anything that isn't written once or also in the log.
- **Keeping blocks of 4 pages.** A smaller tree, but copying a block on a write to one of its pages would read the other three first.
- **The free map in a copy-on-write tree, with its root next to the map's.** Writing it needs space from it, so it would change while it is written, and needs a way to settle that. Two fixed copies don't.
- **Rebuilding the free map by walking the tree on a restore.** Nothing extra is written, but every hard-disk boot would read the whole tree.

## Open questions

- **The size of the cache.** 64 pages is a guess, to be measured with the tests' working set.
- **Releasing a very large region.** Whether the walk in section 9 needs to be done in parts.
- **Raising the 4 GiB limit.** The bitmaps in section 5 grow with it, and they live in memory that must be mapped when the store starts.
