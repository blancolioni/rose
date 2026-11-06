# 9. Files open readers

- **Status:** Proposed
- **Date:** 2026-10-28
- **Related:** [ADR 0010](0010-resources.md) (resources), #389 (implementation), #390 (writers), #331 (`cat`), #344 (petal binds `in File`), #347 (`Truncate`), #141 (shell-owned commit), #215 (seeks capped at the size), [ADR 0003](0003-directories-files-and-users.md) (directories, files and users), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), #726 (`Stream_Writer` renamed `Writer`), #338 (redirection), #735 (an editor)
- **Amended by:**
  - #725, where `File_Reader` is renamed `Releasable_Reader`, `Seekable_File_Reader` is renamed `Seekable_Reader`, and `Stream_Reader` is renamed `Reader`. Nothing in them is specific to files. Sections 1 to 3 keep the old names.
  - #390, which decides how a file is written (section 4). Section 4 uses the new names, and #726's `Writer` for `Stream_Writer`.

## Context

A file is a stream. `interface File : Stream_Reader, Seekable` adds only `Size`, and `Writable_File` adds `Stream_Writer`. The file server keeps one position for each file, and every client of the file reads, writes and seeks at that position.

So readers interfere with each other:

- **`cat` seeks first.** Petal reads `.petalrc` at startup, which leaves the file's position at its end, so `cat .petalrc` printed nothing until `cat` began with `Seek (0)` (#331).
- **`Directory.Read_File` seeks too.** The directory server seeks the file to 0, then returns the file's own `Stream_Reader`. A second `Read_File` of the same file, while the first reader is still reading, moves the first reader back to the start.
- **The tests seek** before they read back what they wrote.
- **Two programs that read one file at the same time** read alternate parts of it, and neither can tell.

A seek only makes the problem less likely. Nothing a reader can do keeps another reader from moving its position.

isofs already works differently. Its `Read_File` makes a reader for each call, with its own position, in a table of open files. But nothing ever frees a reader, so the table fills up.

## Decision

### 1. A file opens readers

A file is no longer a stream. It is something that readers are opened from:

```ada
interface File is
   function Size return Storage_Count;
   function Open_Reader return interface File_Reader;
   function Open_Seekable_Reader return interface Seekable_File_Reader;
end File;

interface File_Reader : Stream_Reader, Resource is
end File_Reader;

interface Seekable_File_Reader : File_Reader, Seekable is
end Seekable_File_Reader;
```

- **Each reader has its own position,** which starts at 0. Reading through one reader never moves another.
- **A reader is sequential.** `File_Reader` is a `Stream_Reader` and nothing more, which is all that `cat`, petal and the installer need. A reader that can seek is a `Seekable_File_Reader`, opened with `Open_Seekable_Reader`. Code that takes a `File_Reader` can be given either.
- **A reader is a `Resource`** (ADR 0010). The server holds each reader's position in a table, and `Release` frees it. After that, the reader's caps fail. The generated client's `Close` calls `Release`.
- **A reader sees the file as it is,** not a copy made when the reader was opened. What a reader sees while the file is being written is part of #390.

### 2. `Directory.Read_File` goes

`Read_File (Index)` is `Get_Ordinary_File (Index)` followed by `Open_Reader`, so the directory interface doesn't need it. Its users open a reader from the file instead:

- petal's startup file;
- `Rose.Directories.Open`, which the installer uses to read executables from the ISO;
- the tests.

isofs implements `Get_Ordinary_File`, which returns a null cap today. The `File` it returns opens readers, as its `Read_File` makes them now, and `Release` frees them.

### 3. The file server

- **Readers are instances.** A reader is an instance of the file server's reader endpoints, with an identifier of its own, which picks its entry in the table. The file's own caps are unchanged.
- **The table is fixed in size,** since servers allocate nothing. `Open_Reader` and `Open_Seekable_Reader` fail with an error when it is full.
- **Seeks are capped at the size,** as the file's own seeks are now (#215).
- **The table persists.** It is in the file process's memory, so a checkpoint saves each reader's position, and a restore brings it back with the caps that reach it.

### 4. A writable file opens writers

Decided in #390. A writable file is no longer a stream either. Writers are opened from it, as readers are:

```ada
interface Writable_File : File is
   function Append return interface Releasable_Writer;
   --  A writer that adds to the end of the file.

   function Rewrite return interface Releasable_Writer;
   --  Empty the file, then a writer that adds to its end.
end Writable_File;

interface Releasable_Writer : Writer, Resource is
end Releasable_Writer;
```

- **A writer has no position.** Every `Write` goes at the end of the file, as it already did. `Rewrite` is `Append` after emptying the file. `>` and `>>` (#338), `copy` and the editor (#735) need nothing else.
- **`Truncate` goes.** `Rewrite` replaces `Truncate (0)`, which is the only length that anything but the tests shortened a file to. This undoes what #347 added.
- **A writer is a `Resource`** (ADR 0010), although it has no position to keep. Petal can hand a writer to a command, as standard output, and end it when the command ends. `Release` frees it and commits nothing.
- **Positioned writes,** a `Seekable_Writer`, wait until something needs them.

**More than one writer at once is allowed.** With no position, writers have nothing to fight over. Each `Write` is atomic and goes at the end, as with `O_APPEND`, so two writers interleave whole writes, which is what a log wants. A `Rewrite` empties the file under every other writer, and under its readers; that is the caller's choice.

**What a reader sees** while the file is written:

- **A live `Writable_File`:** the writes as they are made, as section 1 says ("the file as it is"). A reader past the end after a `Rewrite` moves back to the new end, as a seek past the end does (#215).
- **A version** (ADR 0026): the file's readers see what was last committed. Readers opened from the version see the version.

Isolation comes from versions, not from writers. This also settles shell-owned commit (#141): a version is how a file is replaced, and the command commits it. `copy`, the editor and `>` call `Rewrite` on the version, write, then `Commit`.

**`Size`** is the current length of a live file, and nothing on a live file is uncommitted. A version's `Size` is the version's own. The `Size` of a file that has a version is the committed length.

## Consequences

- **Readers stop interfering.** `cat` and the tests lose their seeks, and two programs can read one file at once.
- **Readers must be released.** A program that opens a reader and never releases it holds a slot in the file server until the server's table is full. Generated user-target clients could release a reader when the program closes it, but the program has to close it first.
- **The interfaces change:**
  - **`File`:** no longer a stream, and it gains two operations;
  - **`File_Reader`** and **`Seekable_File_Reader`:** new;
  - **`Directory`:** loses an operation.

  Every client and server of these interfaces changes with them, and so does idle's snapshot of Rose's interfaces.
- **`File` can be given to a command without letting it disturb other readers.** Today a command given a read-only `File` can still seek it, and so move the position of everyone else who reads it.
- **Writers must be released too,** and the file server keeps a table of them, fixed in size like the readers'.
- **`Writable_File` changes:** it no longer inherits `Stream_Writer`, it loses `Truncate`, and it gains `Append` and `Rewrite`. Its users change with it: petal, `copy`, the tests, and the file server. This comes after #726 renames `Stream_Writer`.
- **Writes can't be made in the middle of a file** until a `Seekable_Writer` is added. Nothing needs that yet.
