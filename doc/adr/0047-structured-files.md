# 47. Structured files: a server for each kind

- **Status:** Proposed
- **Date:** 2026-11-26
- **Related:** #1147 (this decision), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed streams, IDL record types), [ADR 0009](0009-files-open-readers.md) (files open readers), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (rights are types, `or` lists), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), [ADR 0042](0042-configuration-as-a-tree.md) (configuration as a tree), [ADR 0045](0045-opening-files-by-name.md) (opening files by name), #140 (core stream interfaces and unit types), #731 (configuration server), #732 (`Rose.Configuration` library), idle#113 (record types)

## Context

Files aren't all alike. Most are plain text. Some are arrays of one record type, such as a log of events or a table of scores. Some are configuration, with keys, values and child keys. Some are databases, with many writers, queries, and commits while they stay open.

On Unix every file is bytes. A program learns what the bytes mean from the file's name or its first few bytes, and brings its own parser. Two programs reading the same format can disagree about it, and nothing checks that a file is the kind its reader expects.

Rose already avoids this in two places. A program finds out what it was given from the interfaces its cap implements, not by asking (ADR 0012, section 6). A configuration is a `Tree` whose leaves are single values, not a file in a format (ADR 0042).

### What exists today

- **`Files.File`** has `Size`, `Open_Reader` (from `Streams.Readable`) and `Open_Seekable_Reader`. A file is bytes, and opens readers (ADR 0009).
- **`Files.Directory`** is a `Common.Tree`. Its entries are directories, ordinary files and special files.
- **A configuration leaf is one value as text** (ADR 0042, section 3). `Rose.Configuration` (#732) will parse it, with one spelling for each type: `true` and `false`, decimal integers, `#rrggbb` colours.
- **The IDL's only types are enumerations and integer ranges.** ADR 0002 (section 10) plans record types for the elements of typed streams (idle#113), and #140 plans versions, unit types and a canonical text rendering for each.
- **`Ada.Text_IO` can open files by name** (ADR 0045). `Sequential_IO` and `Direct_IO` aren't in the run-time system.

## Decision

### 1. A file's kind is the interfaces its cap implements

- **Each kind has a server**, which implements an interface descending from `Files.File`: `Text.Lines` for text, `Records.Sequence` for records, and so on. `Files.File` is one interface among several, and is the least of them.
- **A program asks for the kind it needs** in its signature, with ADR 0012's `or` lists, such as `Input : in Files.File or Text.Lines`. Petal passes the most capable form it holds, and the program tests for it with a membership test.
- **Nothing guesses a file's kind** from its name or its contents. The kind is recorded with the directory entry when the file is created, and the directory hands out the kind server's view of the file.
- **The directory gets the view from the kind's server,** giving it the bytes file to work on. The directory server holds a cap to each kind server it knows about. An entry whose kind it doesn't know is given out as a plain `Files.File`.

### 2. Every kind keeps a bytes view

- **Every kind answers `Files.File`,** with a canonical serialisation of its contents. Copying, backup, export and the host target (#1101) work on any file through this view, without knowing its kind.
- **For text, records and configuration, the bytes are how the file is stored.** The kind server keeps no other copy, so the bytes always match the structure, and writing the bytes back gives the same file.
- **For a database, the bytes are an export** (section 8), which the database server can import.
- **The canonical form is the kind server's to define,** and is written in the kind's interface package.

### 3. One small set of value kinds

The values in structured files come from one closed set, shared by record fields and configuration leaves:

- `Boolean`, `Integer`, `Float`, `String` (UTF-8), and `Enumeration`, whose literals are listed where the value's type is described.
- **The text spelling of each is ADR 0042's** (#732), so a value reads the same in a configuration leaf, in a record file's bytes view, and in a typed stream's text rendering (#140).
- **Unit types** from #140, such as `Bytes` and `Duration`, are added to this set when #140 defines them. Each is stored as one of the kinds above, with its unit.
- **No caps.** A value is data. A cap in a configuration is a cap entry of the configuration server (ADR 0042, section 4), not a value.

### 4. A record is a flat list of named values

- **A record type is a list of fields,** each with a name and a value kind from section 3. No nesting, no variants, no access types, no arrays inside a record.
- **This is the whole type system.** It is also the shape of the IDL record types that ADR 0002 plans for stream elements (idle#113): an IDL record type, when idle has them, is one of these lists with a name and a version (#140). The IDL doesn't need more for files.
- **A structured file describes itself.** Its bytes start with its record type's fields, and, when there is one, the name and version of the IDL type it was written as. A program can read any record file without the IDL that defined it.

### 5. Record files

- **`Records.Sequence`** gives `Row_Count`, `Field_Count`, `Field_Name`, `Field_Kind`, and a getter for each value kind, such as `Get_Integer (Row, Field)`. A writable descendant adds `Append` and `Set`, and is `Common.Transactional`, so a command commits it (ADR 0026).
- **Typed access is an Ada generic,** like `Ada.Direct_IO`, instantiated with the program's record type and the conversion between that type and a row. Opening a file through it compares the file's fields with the record type's, by name and kind, and raises on a mismatch. Unlike `Direct_IO`, it never reads a file as a type it wasn't written as.
- **Idle generates the conversion** from an IDL record type, once it has them (idle#113). Until then a program writes it by hand.

### 6. Text

- **`Text.Lines`** gives `Line_Count` and `Get_Line (Index) return String`. Text is UTF-8, as configuration leaves are (ADR 0042, section 3).
- **A writable descendant** replaces, inserts and deletes lines, and is `Common.Transactional`.
- **An ordinary file is text unless it was created as something else.** A file without a recorded kind is served as text by the text server, and as a plain `Files.File` where there is none, such as in `isofs`.

### 7. Configuration leaves can be typed

- **ADR 0042 holds:** a configuration is a `Tree`, and a leaf is one value, stored as its text spelling.
- **`Values.Value`,** a descendant of `Streams.Readable`, adds `Value_Kind` and a getter for each kind. The configuration server (#731) serves its leaves as `Values.Value`.
- **The library tests for it.** `Rose.Configuration` (#732) uses a leaf's `Values.Value` getters when the leaf implements it, and otherwise parses the leaf's text. A directory of one-line files stays a valid configuration.

### 8. Databases are objects, not files

- **A database is a persistent process** that keeps its data in memory and is checkpointed with everything else. It needs no file format of its own.
- **It is `Common.Transactional`,** committing while it stays open, which `Close` in ADR 0045 can't do.
- **A directory can name one,** as an entry that isn't an ordinary file, as a special file is named today (ADR 0035).
- **Its bytes view is an export,** for backup and moving between systems (section 2).
- **Its query interface waits for a program that needs one.**

### 9. Order

1. Value kinds, and `Values.Value` for configuration leaves (sections 3 and 7), with #731 and #732.
2. Text (section 6), with kilo as the first user.
3. Records (sections 4 and 5), with idle#113.
4. Databases (section 8), when a program needs one.

## Consequences

- **A program can't misread a file's kind.** It asks for an interface, and gets it or doesn't.
- **Every file can still be copied and backed up** without knowing its kind, through its bytes view.
- **The IDL gains flat records and nothing more,** and these were already planned for typed streams. A typed stream of `File_Info` and a record file of `File_Info` describe their elements the same way.
- **Directories record a kind for each entry,** and the directory server holds caps to the kind servers. A file copied in from outside, such as from `isofs` or the host, arrives as text or plain bytes, and is given a kind when it is created as one.
- **New interfaces only,** which fits the interface rules from 0.4.1. `Files.File` and `Common.Tree` don't change.
- **`Sequential_IO` and `Direct_IO`** can be added to the run-time system over `Records.Sequence`, with the same check at open, after ADR 0045's `Text_IO`.

## Alternatives considered

- **A format in the bytes, and a parser in each program,** as on Unix. Every reader would carry its own parser, and a file's kind would be a guess.
- **One store that knows every layout.** One server would grow with every kind, and a new kind would mean changing it.
- **A type system in the IDL** for files, with nesting, variants and arrays. Every file would need its IDL to be read, and the IDL compiler would grow for a need that flat records meet.
- **Files typed only by the program,** as `Ada.Direct_IO` is, with no description in the file. Nothing would catch a file read as the wrong type, and a file would be unreadable without the program that wrote it.
- **No bytes view for structured kinds.** Copying, backup and export would each need to know every kind.
- **Petal wraps a file in its kind's view** when it binds an argument, rather than the directory. Programs that open files by name (ADR 0045) would get only bytes, and the kind would have to be recorded somewhere other than the entry anyway.

## Open questions

- **Schema evolution.** How a program built for version 2 of a record type reads a file written as version 1: a conversion, a defaulted new field, or a refusal.
- **Where the kind is recorded,** and what names a kind: an interface name, or a kind server's name in the directory server's table.
- **Large values,** such as an image in a configuration leaf. ADR 0042 reads them through `Get_Reader`, and they may need a value kind of their own.
- **Colours,** which ADR 0042 spells `#rrggbb`: a value kind, or a spelling of `Integer`.

## Follow-up work

- #731 and #732: `Values.Value`, and the value kinds and their spellings (sections 3 and 7).
- #140: unit types join the value kinds (section 3).
- An issue for `Text.Lines`, its server, and kilo as its first user (section 6).
- An issue for record files: the self-describing header, `Records.Sequence`, and the typed generic (sections 4 and 5), after idle#113.
- An issue for recording a kind with each directory entry (section 1).
- The Rose Book: `files.adoc`, on what kinds of file there are.
