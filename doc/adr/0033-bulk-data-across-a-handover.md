# 33. Bulk data across a handover

- **Status:** Proposed
- **Date:** 2026-11-12
- **Related:** #885 (this decision), #824 (replacing a running process), #867 (`file` replaceable), #760 (no size limit for `file`), #881 (saved states), #879 (deferred calls in a handover), #246 and #305 (disk blocks on first write), [ADR 0003](0003-directories-files-and-users.md) (one process per file), [ADR 0014](0014-banks.md) (banks), [ADR 0025](0025-shared-regions.md) (shared regions), [ADR 0028](0028-paging-to-the-log.md) (paging to the log), [ADR 0032](0032-replacing-a-running-process.md) (replacing a running process)

## Context

A new version of a server takes over from the old one by reading its state stream (ADR 0032, section 3). For most servers the state is names and caps, and streaming it is cheap. For `file` it isn't.

### What exists today

- **Every file is its own process** (ADR 0003, section 2), and holds its contents in its own memory: `Contents`, a fixed 64 KiB array in `.bss`. #760 removes the limit.
- **Replacing `file` replaces every file process.** Each one streams its whole contents to its new version (#867), which writes them into its own memory. An update therefore reads and rewrites every byte in the filesystem, and the next checkpoint writes all of it to the log again. The time and the log space grow with the size of the filesystem, not with the size of the change.
- **A process's private pages are keyed by process.** Mem keeps a process's private, writable pages in memory or in the log (ADR 0028), and a saved page is named by mem's process number and its virtual page (`Rose.Checkpoints.Page_Reference`).
- **`Process_Banks.Hand_Over` moves every sub-bank** of the old version's bank under the new version's (ADR 0032, section 6). Whatever is reserved from a sub-bank survives the old version's end.
- **A shared region (ADR 0025)** maps one store page into several processes. Its pages are keyed by store page, not by process, but a shared page that anything maps is never paged out (ADR 0028, section 8).

### What's needed

- A handover whose cost doesn't depend on how much data the server holds.
- Data that is handed over stays pageable, like any other private memory.
- The handover's rules still hold: the old version is unchanged until the swap, and goes on as before if the handover fails (ADR 0032, section 7).
- A saved state (ADR 0032, section 10) still carries everything, because a file can't hold caps or segments.

## Decision

### 1. Bulk data lives in a data segment

A server that holds a lot of data keeps it in a **data segment**: an ordinary private, writable segment over a region that is reserved from a **data sub-bank** of its process bank.

- **It is private memory,** paged to the log like any other (ADR 0028). Nothing is pinned, and it uses none of mem's shared table.
- **The region is in a sub-bank** so that `Hand_Over` moves it to the new version's bank with nothing new in the store, and the old version's end doesn't release it.
- **A server can have more than one,** for data that grows independently, within the process's segment limit.

The rest of a server's state stays where it is, on its heap, and is streamed.

### 2. Mem moves the data segment at the swap

A data segment isn't streamed. Mem moves it from the old version to the new one during `Replace_Process`, in the step where it already swaps its records of the two processes (ADR 0032, section 6, step 2).

- **The new version asks for it.** As it reads the state, the new version names each data segment that it takes over, and the base at which it wants it, in a window of its address space that is kept for data segments. A segment it doesn't ask for is released with the old version.
- **Mem re-keys the segment's pages,** in memory and in the log index, from the old version's process number and address to the new version's. No page is read, written or copied. The next checkpoint records the segment under the new version.
- **The new version gets the data only after the swap,** as it gets the deferred calls' reply caps (#879). Until then it can't touch it, so a failed handover leaves the old version's data as it was, and no rule asks the new version not to write it.
- **No checkpoint runs during a handover** (ADR 0032, section 5), so a checkpoint never sees a segment half moved.

A handover then costs the size of the state stream, whatever the size of the data.

### 3. What can live in a data segment

Only **flat data with a format**: bytes, or records of plain scalars with an explicit layout, behind a header that gives a format version. Nothing in it may point anywhere.

- **The new version maps it at a base of its own,** so an address in it would be wrong.
- **The new version is a new program** (ADR 0032). An access value, a tag or a dispatching table would point into the old version's code or heap, and a record laid out by the old version's compiler need not match the new one's.

So heap structures stay on the heap, and are streamed: containers, `Unbounded_String`, and anything that allocates. A server pays the flat-format discipline only for the data that is large enough to matter.

The data's format is the server's, and follows ADR 0032's rule: a new version reads its predecessor's format. If it changes the format, it converts the data once, in place or into a new data segment, as a deliberate act.

**`Rose.Region_Buffers`,** a library package, gives growable byte storage over a data segment: `Length`, `Read` and `Write` at an offset, `Append` and `Truncate`, with the length in the segment's header. It is nearly as easy to use as an `Unbounded_String`, and can be handed over. The segment's region is reserved large, and costs blocks on the device only as its pages are written (#246, #305).

### 4. A saved state carries the bytes

A saved state (ADR 0032, section 10) can't hold a segment, so a save writes each data segment's contents into the stream, after the server's own state. A handover doesn't need them, because the segment moves.

- **The stream always names each data segment.** A restored instance reads the bytes that follow into a fresh data segment of its own. A new version in a handover asks for the segment by name instead.
- **A handover mustn't pay for the bytes.** The old version can't tell a handover from any other call of `State`: they are the same operation (#879). How it avoids writing them is open (see Open questions).

### 5. Different files, different programs

Not every file is the same, and they don't all need the same program. `Writable_File` is the contract, and any program that serves it can serve a file.

- **A small file,** such as a text file, can keep its contents on its heap, in an `Unbounded_String` if it likes, and stream them in a handover. That is cheap for small files.
- **A large file** keeps its contents in a data segment, and isn't copied in a handover.
- **The directory launches the program,** as it launches `file` today. It holds a launch cap for each file program, not one.

Clients don't see which program serves a file. They use the interfaces, and test for a descendant interface where they need more (ADR 0032, section 6).

### 6. Not for 0.4.0

Nothing here changes an interface that freezes with 0.4.0. Mem's segment move and `Rose.Region_Buffers` are new, and how a stream carries data segments is `Rose.Replacement`'s business.

A `file` from 0.4.0 streams its contents (#867). A later `file` reads that stream, and writes the contents into a data segment once. After that, no update copies them.

## Consequences

- **Replacing a file costs the same whatever its size,** and the update writes nothing to the log for its contents.
- **Mem gains an operation,** used only inside `Replace_Process`, that re-keys a segment's pages from one process to another, and a window for data segments in each process's address space.
- **A server chooses, for each piece of data,** whether it lives on the heap and is streamed, or in a data segment and is moved.
- **`file`'s contents move from `.bss` to a data segment,** which also lifts its 64 KiB limit (#760).
- **The directory can hold several file programs,** and has to choose one for each new file.

## Alternatives considered

- **Stream everything,** as ADR 0032 does. It is simple and correct, but an update copies the whole filesystem.
- **Shared regions** (ADR 0025). The new version would map the old version's region shared, and see its pages with no copy. But a shared page that anything maps is never paged out (ADR 0028, section 8), so every file's contents would stay in memory for as long as the file exists. Mem's shared table has 2,048 pages for the whole system, and a fault that would overflow it fails the process. Each segment over a region costs about seven of mem's caps, out of 4,096 for the whole system.
- **`Bank.Take` of single regions** into the new version's bank (ADR 0032, alternatives). The sub-bank already moves with `Hand_Over`, and the region alone doesn't carry the process's private pages, which hold the data.
- **A copy-on-write clone of a region** in the store. The store has no way to share blocks between regions, and the pages that matter are in mem and the log, not in the region.
- **Replace only the code,** and keep the old process's data pages. The new version would inherit the old one's whole layout, heap included, which ADR 0032 rejects.
- **Never replace file servers.** Old code would then run for as long as the file exists, with any bug it has.
- **Replace a file server lazily,** at its next open. It spreads the cost out, but still copies.
- **One server for every file.** It is a much larger change, against ADR 0003, and the data would still have to cross a handover.

## Open questions

- **Keeping the bytes out of a handover's stream.** One way is a stream that `Rose.Replacement` produces as it is read, with the data segments last: a new version stops reading before them, so they are never written. Another is to write the bytes only from `Rose.Replacement.Save`, which the server calls itself, and never from `State`. A `Saveable` holder outside the server would then get a state without its data.
- **How the directory chooses a file's program.** The creator could name it, as an argument of `Create_File`, or the directory could choose by name or size, with a default.
- **Moving a file to another program,** such as a small file that grows large. `Replace_Process` could do it with a program that reads the other's format, but it is privileged to the update (ADR 0032, section 1).
- **The data segment window's place and size,** and how many data segments a process may have, within mem's segment limit (ADR 0025, section 6).

## Follow-up work

- Mem: move a data segment from one process to another inside `Replace_Process`, re-keying its pages in memory and in the log index, and the data segment window.
- `Rose.Replacement`: name data segments in the state, ask for them in `Take_Over`, attach them after the swap, and write their contents into a saved state but not a handover's.
- `Rose.Region_Buffers`.
- A file program whose contents are in a data segment, reading 0.4.0's `file` stream.
- The directory: several file programs, and a choice for each new file.
