# 28. Paging changed memory out to the checkpoint log

- **Status:** Proposed
- **Date:** 2026-11-05
- **Related:** #761 (this decision), #759 (heaps without a 1 MiB cap), #760 (files without a size limit), #356 (consolidation), #246 and #305 (store blocks on first write), #200 (the read-only page cache), [ADR 0025](0025-shared-regions.md) (shared regions), [ADR 0014](0014-banks.md) (banks)

## Context

A persistent process's changed pages can leave memory only through a checkpoint. The book's *Persistence* chapter says so under *Where Memory Lives*:

- **The swap partition holds only what is written to a region on purpose:** program images, and the pages of shared segments that `mem` writes back when their last mapping goes (ADR 0025).
- **When `mem` needs a page,** it can evict only a cached page of a read-only segment that nothing maps (#200), since it can read that again from its region.
- **If there is no such page,** `Mem.Processes` prints `no page` and returns, and the faulting process is never resumed.

So the changed pages of every persistent process must fit in physical memory between checkpoints. That was safe while heaps were capped at 1 MiB and files at 64 KiB, but #759 and #760 remove both caps.

The book's *Future Work* states the question this ADR answers: does swap become part of the restored state, or stay a cache that a restore can't trust past the last checkpoint?

The constraint is that **a restore must see exactly the state of the last committed checkpoint.** A changed page can't be written to the block the store's committed map names for it, or a restore reads a page newer than the rest of its checkpoint. ADR 0025 rejected `Region.Put` at checkpoints for the same reason.

Three facts about the log shape the decision:

- **Each entry holds only the pages changed since the one before.** So a restore reads every entry, from the first to the last, and the last copy of a page that it reads wins. A restore gets slower as the log grows.
- **The kernel's, the store's and `mem`'s tables are complete in every entry.** Only the pages are spread over several entries.
- **A page saved by a checkpoint and unchanged since has a copy in the log.** If `mem` could find that copy, the page would be clean. It could be dropped from memory at no cost, and read again from the log.

Finding a page's latest copy needs an index from each page to a log page. That index grows with the whole persistent state, not with what changed recently, so it can't simply be a table in `mem`'s memory. `mem`'s static data is 396 KiB today. At 4 bytes for each page, an index of 256 MiB of saved pages would be 256 KiB, and an index of 1 GiB would be 1 MiB, all taken from `Boot_Module_Pages`.

## Decision

### 1. The log is the backing store for process memory

A page of a process's private, writable memory lives in one of three places:

- **in memory**, in a physical page that `mem` maps;
- **in the log**, as the latest copy of it that any entry holds, committed or not;
- **nowhere yet**: it reads from its segment's region, or as zeroes, as today.

Read-only segments keep using the page cache (#200), and shared segments keep using the shared table (ADR 0025). Neither of them is paged out to the log (see section 8).

### 2. The log index is in the log

The index from pages to log pages is kept in the log itself, as a tree for each process, written at each checkpoint:

- **Its shape follows the i686 page tables.** A process's *index directory* is one page of 1,024 entries, each the log page of an *index leaf*, or 0. An index leaf is one page of 1,024 entries, one for each page of 4 MiB of the process's address space. Each entry is the log page that holds that page's latest copy, or 0 if the log has no copy. Log page 0 is the log's header, so 0 never names a page of an entry. `log` numbers its pages from 0 across the partition, so one 32-bit number names a page in any entry.
- **Index pages are never changed in place.** A checkpoint writes new copies of the leaves that changed, and a new copy of the directory if any of its leaves moved. Copies in older entries stay valid until consolidation (#356) frees them.
- **`mem`'s process record** in each entry gets the log page of the process's index directory, or 0 if the process has nothing in the log. So the roots of every process's index are in the last committed entry, with the rest of `mem`'s tables.

An architecture with a larger address space gets more levels, as its page tables do.

### 3. What `mem` keeps in memory

- **A table of changes since the last checkpoint.** For each page written to the log since then, it gives the process, the virtual page and the log page. Its size is fixed, and bounded by the work done between checkpoints, not by the size of the system.
- **A cache of index pages.** Index pages are read from the log when a lookup needs them, and are clean, so `mem` drops them under pressure like any clean page. They take physical pages that `mem` manages, not its static data.

To find a page's latest copy, `mem` looks in the table of changes first. If the page isn't there, it reads the process's index directory and then the index leaf, from the cache or from the log.

### 4. An entry is open between checkpoints, and grows in batches

`log` opens an entry when the previous one is committed. It no longer waits for the next checkpoint. `mem` appends to it whenever it pages out:

```
[batch: a header page listing N pages | the N pages]   ← paged out between checkpoints
[batch: ...]
--- checkpoint ---
kernel tables | store tables
[batch: header page | the changed pages still in memory]
index pages | mem tables
shared pages                                            → commit
```

- **A batch has today's format:** one or more header pages (`Rose.Checkpoints.Header_Page`), the last with `Is_Last_Header_Page`, then the pages in the order they are listed. Each page of a batch goes into the table of changes as it is written.
- **At a checkpoint,** `mem` writes its last batch first: the changed pages still in memory. Then it adds the table of changes to the index, and writes the index pages that changed. Its tables come last, since its process records name the new index directories.
- **Index pages are preceded by a header page of their own kind,** which counts them, so that a reader can skip them.
- **After the commit, the table of changes is empty.** Every page it named is in the committed index.

A page that is paged out and not changed again is correct for the checkpoint that commits its entry. To change, it must be faulted back in, and is then written again later in the same entry, or in a later one. The table of changes keeps only its latest copy.

Pages of a process that is gone by the time of the checkpoint are not added to any index. They only waste space in the log.

### 5. Paging out

- **Clean pages are dropped first.** A clean page is either read-only, or has a copy in the log or its region and hasn't changed since. `mem` unmaps it and frees it, with no write.
- **Changed pages are written as a batch** to the open entry, then unmapped and freed.
- **Which pages go:** the least recently used, as far as `mem` can tell. A checkpoint's write protection tells `mem` when a page is first written (`Set_Read_Only`, a write fault, `Set_Writable`). `mem` can also unmap a clean page that is still in memory, and count a fault on it as a use, without reading the page again. Whether the kernel's accessed bit is worth reading is left to the implementation.
- **`mem` keeps a reserve of free pages,** and pages out when the reserve falls below a low-water mark. It doesn't wait until a fault finds no page. Writing a batch goes through `log`, `partition` and `ata`, and `mem` must never wait on a fault that it would have to handle itself.

### 6. When the table of changes fills

- **When it is three quarters full,** `mem` asks the checkpoint server for a checkpoint now. It can't wait for that checkpoint, because the checkpoint needs `mem`, so the request is a message that doesn't wait for a reply.
- **When it is full,** `mem` drops clean pages but writes no more changed pages. A fault that needs a page, when no clean page is left, waits for the checkpoint to empty the table.

So heavy paging brings checkpoints forward, rather than running `mem` out of memory.

### 7. Faulting a page back in

A fault on a page that has a copy in the log reads that log page and maps it.

- **`log` gets a read by log page number** that reaches the open entry as well as the committed ones. `Log_Reader` reads committed entries only, and is for `restore`. The new operation is on the interface that `mem` holds, which only the pager path reaches.
- **A lookup that misses the cache costs up to two more reads:** the index directory and an index leaf. A process's index directory will usually be in the cache.
- **The page comes back clean.** Its copy in the log is still valid until it changes again.

### 8. What is never paged out

- **The boot modules,** including the pager path (`mem`, `store`, `partition`, `ata`, `log`, `checkpoint` and `restore`). They take their memory from the kernel's reservation (`Boot_Module_Pages`), not from `mem`.
- **Shared pages that anything maps** (ADR 0025, section 3). When the last mapping goes, a shared page is written back to its region, as now.
- **Pages the kernel has pinned** for a call in progress: the pages of a buffer that the kernel has mapped into another process's shared slot (#519).

### 9. Restore reads one entry

`restore` reads the kernel's, the store's and `mem`'s tables from the last committed entry only, as they are complete there.

- **`mem`'s process records give each process's index directory.** `Memory_Restore.Restore_Process` takes that log page as well, and `mem` reads the index on demand from then on.
- **`Memory_Restore.Restore_Page` is no longer used** for a checkpoint with an index. No page is read until a process faults on it.
- **Shared pages are still read from every entry,** as ADR 0025 describes, since they have no index. They are few.
- **A checkpoint written before this change,** whose `mem` tables have no index directories, is restored as now: every entry is read, and every page is given to `mem` with `Restore_Page`. Those pages are all dirty, so the next checkpoint saves them all and writes a complete index.

So a restore does about the same work whatever the size of the log, and memory fills as the system runs. The system can also be bigger than physical memory at the moment it is restored.

### 10. During a checkpoint

- **While a checkpoint is being written, `mem` drops only clean pages.** The changed pages are being written by the checkpoint anyway. Persistent processes are write-protected in checkpoint mode, so they can't change pages until it ends, and dropping clean pages is enough to give their faults the pages they need.
- **After the checkpoint, every page it saved is clean,** because its copy in the log is now committed and indexed. Paging out costs no writes until pages change again.

### 11. When memory or log space runs out

- **A fault that finds no page,** after `mem` has paged out everything it can, and with no checkpoint on the way to free the table of changes, fails the faulting process. It no longer leaves the process waiting forever.
- **If the log is full,** `mem` can't write changed pages. It still drops clean ones, and when only changed pages are left, it fails the faulting process. Neither the table of changes nor the index ever names a page that `log` failed to write.

### 12. Interfaces and formats

- **The log interface that `mem` holds:** append a batch to the open entry, and read a page by log page number. Opening an entry after each commit is `log`'s own business.
- **The checkpoint server:** a request for a checkpoint now, which doesn't wait for a reply.
- **`Memory_Restore.Restore_Process`:** also takes the log page of the process's index directory.
- **`Rose.Checkpoints`:**
  - index directory and index leaf pages;
  - a header page for index pages;
  - the index directory in `mem`'s process record, with `Mem_Table_Version` at 2;
  - an entry may hold several memory batches, and `mem`'s tables come after its pages.

  A restore that reads an entry from the start finds each part by its magic number, and skips the pages that a header page counts.

## Consequences

- **Persistent memory is no longer limited by physical memory,** only by the log partition. #759 and #760 can go ahead without making the system hang on `no page`.
- **`mem`'s own memory is bounded:** a fixed table of changes, and a cache of index pages that shrinks under pressure. Neither grows with the size of the persistent state.
- **Restores read one entry,** plus the shared pages, not the whole log. They get faster, and stay fast as the log grows.
- **The log grows faster,** with every page paged out, and with the index pages each checkpoint writes. A checkpoint writes at most one index leaf for each 4 MiB of address space that changed, and one directory for each process that changed. Consolidation (#356) becomes needed sooner. It also becomes easier, because the committed index lists each page's latest copy: consolidation copies those pages and the index into a new entry, and frees the older entries.
- **A fault on a page that was paged out reads the disk,** up to three times on a cold cache: the page, its index leaf and the process's directory. This is the cost of paging, and the reason why section 5 prefers to drop clean pages.
- **Heavy paging brings checkpoints forward** (section 6), so checkpoints come more often under load.
- **`restore`, `mem`, `log` and `checkpoint` change together,** and so does the checkpoint format (section 12).

## Alternatives considered

- **The whole index in `mem`'s memory.** This was this ADR's first draft. It is simpler, but `mem`'s memory would grow with the persistent state, which is unbounded (see Context). It also leaves restore reading every entry to rebuild the index.
- **The location in the not-present page table entry,** as Linux keeps swap entries. That's 4 bytes a page in the kernel's page tables, so it still grows with the persistent state, in kernel memory that can't be paged out. A restore would have to rebuild those entries from the whole log.
- **Shadow blocks in the store.** A changed page would go to a fresh block of its region's backing store. The store would keep a committed map and a current one, and each checkpoint would commit the current map and free the replaced blocks. But the store would need commit semantics that the log already has. A page's latest copy could then be in either the log or a shadow block, and a restore would have to read both to know which is newer. It also depends on #246.
- **Swap as a cache that a restore doesn't trust.** Changed pages would be paged out to the swap partition, but every checkpoint would still have to put each changed page in the log, so it would read the paged-out ones back in first. A restore stays simple, but a checkpoint costs more the more has been paged out, and the cost falls exactly when memory is short.
- **Raising physical memory.** QEMU's 32 MB could be raised, but the limit would still be there, and a system that hangs when it reaches it is the problem.

## Open questions

- **The size of the table of changes,** and the three-quarters mark. They set how much can be paged out between checkpoints before one is brought forward.
- **Shared pages.** A display server's surfaces are large, and ADR 0025 never evicts a shared page that something maps. Paging out shared pages, with an index of store pages, could follow the same scheme. Restore could then stop reading every entry for shared pages.
- **Banks (ADR 0014 and 0015).** A bank may limit how much of its processes' memory is resident, or how much log space it uses, and paging out is how such a limit would be enforced. Who pays for the log space that a page uses is left to that work.
- **Non-persistent processes that `mem` serves.** Their pages could be paged out to the log too, and kept out of the index. Their copies would only take up space until consolidation. Whether that's acceptable, or they need somewhere else, depends on how many there are.
