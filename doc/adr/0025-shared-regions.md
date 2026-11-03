# 25. Mapping a region shared

- **Status:** Proposed
- **Date:** 2026-11-03
- **Related:** #593 (this decision), [ADR 0022](0022-display-server.md) (the display server, which needs it), [ADR 0014](0014-banks.md) (banks), #200 (the read-only page cache), #356 (consolidation)
- **Amended by:** #593's implementation, which needed two small kernel changes (Context), lists the shared pages written back to their regions in each checkpoint (sections 4 and 5), and fails the faulting process when the shared table is full (section 6)

## Context

The display server and its clients share pixel buffers (ADR 0022, section 2). So do the `screen` driver and the holder of the screen surface. Each side maps the same memory and draws into it, or reads it. `Surface.Get_Buffer` returns a `Region` for this, to be mapped "with the operation that #593 adds to `Region`".

#593 assumed that a process can't map anything into its own address space, but it can. Every process that exec starts holds its own `Process` interface (`Rose.Capabilities.Standard.Process_Interface_Cap`). The RTS already uses it to find its heap, and `Process` inherits `Segment.Add_Segment`, which maps a region at an address the caller gives.

The gap is in what the memory server does with a writable segment over a region:

- **A writable page is a private copy.** On the first touch, the memory server reads the page from the region (`Region.Get`) into a new physical page for that process. Writes go to that page and never back to the region, so two processes that map the same region see two different copies.
- **Only read-only pages are shared.** The page cache (#200) keys physical pages by store page, so processes that map the same executable share its text. A writable page is never in the cache.
- **A checkpoint saves pages by process.** Each saved page is listed by process and virtual page (`Rose.Checkpoints.Page_Reference`). A page that two processes map would be saved twice and restored as two pages.

Writing changed pages back to the region instead would not be consistent. The storage server writes a `Region.Put` straight to its device, outside the checkpoint log. After a restore to an earlier checkpoint, the region would hold pages that are newer than everything else in the system.

Nothing about this needs the kernel. The memory server already maps one physical page into several processes, and handles each process's write fault on it separately.

Amended by #593. Two small kernel changes were needed after all. The kernel's `Unmap_Page`, which nothing had called before `Remove_Segment`, never let the memory server run again. And boot modules take their memory from a reservation in the kernel's heap (`Boot_Module_Pages`), which the memory server's larger tables outgrew.

## Decision

### 1. A shared segment is a new flag on `Add_Segment`

`Segment` gets a flag, `Segment_Shared`. A segment with it maps a region's pages themselves, not copies of them:

- **Every process that maps a store page shared sees the same physical page.** A write by one is seen by the others at once.
- **It can be read-only or writable,** by the existing flags. The `screen` driver maps its holder's buffer read-only, and the holder maps it writable.
- **It can map part of a region,** by `Region_Offset` and the segment's size, as today.

Mapping stays on the `Process` interface, because the memory server serves it, and the memory server is what maps pages. The storage server serves `Region`, and can't map anything, so a `Region.Map` would have to forward to the memory server, which `Add_Segment` already reaches. A process maps a region into itself through its own `Process` interface, and its creator can map one into it through the same interface, as exec does today.

### 2. The caller chooses the address

`Add_Segment` keeps its `Virtual_Base`. The memory server refuses a segment that overlaps another of the process's segments.

The user library adds `Rose.Shared_Regions.Map (Region) return System.Address`, which picks a free address in a window that is kept for shared segments, `C000_0000` to `D000_0000` (256 MB, below the stack). It also adds `Unmap (Address)`. A client that maps one buffer per surface uses these, and never chooses an address itself.

### 3. The memory server keeps a table of shared pages

The memory server gets a table of shared pages, keyed by store page, like the page cache:

- **A fault on a shared segment** looks the store page up in the table. If it is there, the memory server maps that physical page. If not, it allocates a page, reads it from the region, adds it to the table and maps it.
- **Each shared page counts its mappings,** and records whether it is dirty.
- **A write fault** makes the page writable in the faulting process only, and marks the shared page dirty. Each process's mapping stays read-only until that process writes. That's the same as a private page today.
- **A shared page is never evicted** while anything maps it, like any other page that is in use.

A non-shared, read-only segment keeps using the page cache. A shared segment always uses the shared table, even when it is read-only. Otherwise a read-only mapping could be given a stale page from the cache while another process writes the shared one.

### 4. A checkpoint saves a shared page once, by store page

A checkpoint saves each dirty shared page once, keyed by its store page, not by a process and a virtual page:

- **Its own header pages.** `Rose.Checkpoints` gets a second kind of header page for shared pages, which lists store page ids. These pages follow the per-process pages in the memory server's part of the log entry.
- **Every mapping becomes read-only** when a shared page is saved, as for a private page, so the next write is seen as a change.
- **The segment's flags are saved,** including `Segment_Shared`, in the process record, as now.
- **Restore puts the page back first.** `Memory_Restore` gets `Restore_Shared_Page (Store_Page, Page)`. The restore server calls it for each saved shared page, after the region table and before the processes run. It puts the page in the shared table, dirty. Each process's shared segment is restored by `Restore_Segment`, and its first fault finds the page in the table.

So the truth for a shared page is in the log, as it is for a private page, and the region on its device keeps the contents it had when it was first mapped. Consolidation (#356) has to keep the latest copy of each shared page, as it does for each private page.

Amended by #593. The shared header pages also list the pages that were written back to their regions since the last checkpoint (section 5), as released, with no contents. A restore reads the shared pages of every entry up to the checkpoint, and the last that it reads for a store page wins. If that one is released, the region has the newest contents, and the restore doesn't use an older entry's copy. Without this, a page that was saved, then written again and written back when its last mapping went, would be restored from the log with its older contents. The last header page of the per-process pages has a flag, `Has_Shared_Pages`, when shared header pages follow, so entries without them are read as before. Consolidation has to keep the latest of each store page's saved and released records.

### 5. Unmapping and releasing

- **`Segment` gets `Remove_Segment (Virtual_Base)`.** It unmaps the segment's pages from that process and drops its count on each shared page. `Unmap` in the library calls it.
- **A process that ends** drops its counts in the same way.
- **When the last mapping of a shared page goes,** the memory server writes the page back to its region with `Region.Put` if it is dirty, and then frees it. A later mapping reads it from the region again. This write is outside the checkpoint, like every other `Put`, but nothing maps the page any more, so a restore that brings back a mapping also brings back the page from the log. (Amended by #593: the next checkpoint lists the page as released, so that a restore after it reads the page from the region, and not from an older entry; see section 4.)
- **Releasing the region** is the storage server's business. Pages that are already mapped stay mapped until they are unmapped. A fault on a page that isn't in memory gets an error from `Region.Get`, and the memory server faults the process, as it does today for a region that has gone.

### 6. Limits

- **A process has at most 8 segments** today (`Max_Segments`, `Max_Saved_Segments`). A display client with several windows, and the display server with one mapping per client surface, need more. Both limits go to 32.
- **The shared table** is sized like the page cache. A mapping that would overflow it fails, and `Add_Segment` reports that. (Amended by #593: the memory server is built for the kernel target, whose handlers can't report a failure. The table has 2,048 pages, and a fault that would overflow it fails the process.)

What the segment limit costs, from GNAT's representation of `mem-processes.adb` (`-gnatR2`):

- **A segment record is 76 bytes**, 32 of them the open `Region` client. The rest of a process record is 100 bytes.
- **The process table is static,** sized for `Max_Processes` (64), whatever is in use. Today it is 44 KiB, within the memory server's 396 KiB of static data.
- **A saved process record is one page** while it has at most 101 segments: 32 bytes of header and 40 bytes for each segment.
- **A fault scans the process's segments that are in use,** not the maximum, so it doesn't get slower as the limit rises.

| Segments | Process record | Table, 64 processes | Added to mem's static data | Saved record |
|---|---|---|---|---|
| 8 (today) | 708 B | 44 KiB | — | 1 page |
| 32 (this ADR) | 2,532 B | 158 KiB | +114 KiB | 1 page |
| 64 | 4,964 B | 310 KiB | +266 KiB | 1 page |
| 101 | 7,776 B | 486 KiB | +442 KiB | 1 page, full |
| 128 | 9,828 B | 614 KiB | +570 KiB | 2 pages |
| 256 | 19,556 B | 1.19 MiB | +1.15 MiB | 2 pages |

**The memory server's caps limit segments more than the table does.** Each segment over a region keeps its own open `Region` client, which is seven caps in the memory server: the interface cap and the six it fetches. The memory server has one cap table, of at most 4,096 caps (ADR 0023), for every process's segments.

A rough count from the code is about 28 caps per process: three segments over regions, the kernel-process client, and the heap, server and interface caps. That is about 1,800 caps for 64 processes, which leaves room for about 330 more segments over regions in the whole system, whatever `Max_Segments` is. This figure is from reading the code and hasn't been measured.

So a segment for each package (ADR 0017) needs two changes beyond a larger limit:

- **One open client for each region,** shared by the segments over it, with a count. A segment over a region that is already open then costs no caps. Every package in ADR 0017's library region would share one client.
- **Segment records from a pool,** past about 100 segments, so that the memory they take follows the segments in use rather than 64 times the limit. The saved record would need a second page.

### 7. Interfaces

- `interfaces/segment.idl`: the `Segment_Shared` flag, and `Remove_Segment`.
- `interfaces/memory_restore.idl`: `Restore_Shared_Page`.
- `interfaces/surface.idl`: `Get_Buffer`'s comment says to map the buffer with `Add_Segment` and `Segment_Shared`, through `Rose.Shared_Regions`.

## Consequences

- **No kernel change.** The work is in the memory server, the restore server, the checkpoint format and a small user library. (Amended by #593: two small kernel changes; see Context.)
- **The checkpoint format changes,** with a new header kind and larger process records. Old checkpoints restore, because they have no shared pages and their process records are a prefix of the new ones.
- **The display server, the `screen` driver and the console on a surface (#583, #582, #653) are unblocked.**
- **`Region.Get` and `Region.Put` don't see a shared mapping.** While a region is mapped shared, `Get` returns what is on the device, which can be older than the mapping. A program that shares a region should only use it through mappings. The comment on `Region` says so.
- **Pages can be shared between processes for other uses too,** such as a ring buffer between a driver and a server.

## Alternatives considered

- **`Region.Map` on the storage server,** as #593 suggested. The storage server can't map pages, so it would forward to the memory server, and a process would need a cap that lets the storage server reach its address space. `Add_Segment` already gives the memory server everything it needs.
- **Writing changed pages back to the region at each checkpoint.** It keeps the region current, but `Put` is not part of the checkpoint (see Context), so a restore would see shared pages newer than the checkpoint. It also writes every dirty page twice.
- **A new kernel object for shared memory.** It duplicates what the memory server already does with physical pages, and the kernel would have to save it at a checkpoint.
- **The memory server choosing the address.** It would need a reply with the address and its own window allocator, and the memory server would have to know which ranges each process uses for what. A library in the process, which knows its own layout, can do it as well.

## Open questions

- **Copy on fork.** Rose has no fork, so a shared segment is never copied. If one is added, a shared segment stays shared in the child.
- **Sharing open region clients.** It isn't needed for 32 segments, but it is for ADR 0017's segment per package (section 6). It is follow-up work, in its own issue.
- **Who is charged for a shared page** in a bank (ADR 0014). The region's bank pays for the region's storage. The memory a shared page takes isn't charged to anyone, as with every page today.
