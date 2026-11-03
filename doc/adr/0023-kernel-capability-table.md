# 23. The kernel capability table

- **Status:** Proposed
- **Date:** 2026-11-02
- **Related:** [ADR 0001](0001-capability-revocation.md) (revocation), #286 (the table, in SPARK), #116 (per-grant revocation), #277 (proving Rose with gnatprove), #281 (`Rose.Invocation` and `Rose.Capabilities.Layout`, proved)

## Context

The kernel's checks on capabilities are Rose's central security mechanism. They are in `Rose.Kernel.Processes` (`rose-kernel-processes.adb`, 2,571 lines), together with console output, debugging, page-fault handling and process management: `Has_Cap`, `Get_Cap`, `Cap_Layout`, `Load_Cap`, `Create_Cap`, `Delete_Cap`, `New_Cap`, `Copy_Cap`, `Set_Cap`, `Update_Cap`, `Rescind_Cap` and `Is_Valid_Entry`. About a hundred places in 17 files call them, all through functions that take a process id.

What each process has:

- A set of active capability numbers, and `Last_Cap`, the highest that has been used. Numbers run from 1 to 4096.
- Up to 16 pages of 256 capabilities each. A capability is a 16-byte `Capability_Layout`: a 64-bit header (type, access flags, a 24-bit rescinded count, an endpoint index and an identifier) and a 64-bit payload. A page is allocated from the kernel heap when the first capability in it is created, and is read and written through an address.
- A cache of 16 layouts in front of the pages, with a use count for each. It is written back to its page when it is replaced.
- A table of the 64 endpoints that it serves, each with its own send capability.

What validity is. A call through an endpoint capability is valid if the capability's rescinded count is the same as that of the send capability of the endpoint it names, in the process that serves it (`Is_Valid_Entry`). Rescinding a capability adds one to its count. The count is 24 bits, so a capability that has been rescinded is valid again after 2²⁴ more rescinds.

Four things about this make it hard to prove, and hard to be sure of.

1. The checks are mixed with what is not SPARK: console output (`Create_Cap` reports that it is out of capabilities, and the cache traces itself behind a constant), heap allocation, and access through addresses.
2. The kernel is built with checks off (`-gnatp`), so a number outside the table is not an exception but a read or write of whatever is next to it. `Has_Cap` guards the upper end of a capability number with `Last_Cap`, but indexes the active set with the number itself, which the invocation path takes from the process that is calling, and which can be 0.
3. The cache is a second copy of part of the table that has to agree with the pages. Nothing is cached that is not on a page, and a page can be read at once, so it holds the table's state in two places for the sake of a lookup that is not slow.
4. #116 changes this code: a derived capability needs a record of what it was derived from, and a capability is valid only if its count and its ancestors' counts match. Whether the layout has room for it is #116's to settle (ADR 0001 repacks the header). Whatever that record is, it will be in the table, so the table's shape should be settled before it.

What does not constrain it. A checkpoint does not save the table as it is held. The snapshot writes the capabilities, through `Cap_Layout`, into its own page format, and a restore puts each one back at its index with `Set_Cap`, so the representation can change without the log format changing.

## Decision

### A package of its own, in SPARK

`Rose.Kernel.Cap_Tables` holds the table. There is one `Cap_Table` in each process entry. Its operations take a table, not a process id, and do no output.

| Operation | What it does |
|---|---|
| `Has_Cap`, `Layout` | For any number, including 0 and one that is past the table: no capability, or the empty layout. They do not fail. |
| `Create` | A number that was not in use, which is now, with the empty layout; or 0 if the table is full. It does not say so: the caller does. |
| `Set`, `Update`, `Delete`, `Rescind` | Require that the capability is active. |
| `Restore` | Put a capability back at the number it had, as a restore does. |

`Rose.Kernel.Processes` keeps the functions that take a process id, with the names and signatures that they have, so that the call sites do not change. They check what the table requires, and a capability number that is not active is `Invalid_Capability`, as it is for a call. They print what is printed now.

### The cache goes

Pages are read and written directly, and the cache and its use counts are removed. Reading an inactive capability no longer allocates a page for it. This is its own change, before the proof, so that the cost of removing it is seen on its own: the time that `make test` takes, and what the boot takes, before and after. If it is measurably slower, a cache returns as a layer inside the package, which the proof then covers; it will not be put back as it was.

### The pages stay on the heap, behind two primitives

Pages are still allocated from the kernel heap, and the address of each is held in the table as a value that SPARK does not look into. The access through that address is `Read_Slot` and `Write_Slot`, which are not in SPARK, a few lines each, and have contracts that say what they do: a slot reads as what was last written to it, and a write changes one slot. Everything else is proved from them. Allocating a page is not in the package: the functions that take a process id do it before an operation that needs the page, which is what `Load_Cap_Page` does now.

### What is proved

- **P1.** No operation reads or writes outside the table, whatever number it is given.
- **P2.** `Create` gives a number that was not in use, and it is active and empty afterwards. `Delete` makes a capability inactive. Neither changes any other capability.
- **P3.** `Rescind` adds one, modulo 2²⁴, to the count of that capability, and changes nothing else.
- **P4.** Once the send capability of an endpoint has been rescinded, a call through a capability that had the old count is not valid. The wrap-around is stated and not left out: after *k* rescinds of the send capability, a capability with the original count is valid if and only if *k* is a multiple of 2²⁴.
- **P5.** An invariant of the table: `Last_Cap` is at most 4096, and an active capability is at most `Last_Cap`.
- **P6.** `Read_Slot` and `Write_Slot` as above. It is the one place that the proof is only as good as an assumption, and a short one.

`Is_Valid_Entry` becomes a function of two layouts, the one in the call and the send capability of the endpoint, and it is proved to be that their counts are equal. The endpoint table that finds the send capability stays in `Rose.Kernel.Processes`.

### What is not decided here

The slot type is private to the package, and so is the number of slots in a page, which is the size of a page divided by it. #116 may make a slot larger than a layout, with room for a record of what the capability was derived from. That is #116's decision, and an amendment to [ADR 0001](0001-capability-revocation.md), and the package is arranged so that it can change without its callers knowing.

### Order

The table is extracted, and its semantics proved as they are, before #116. #116 is then a change to a table that has proofs, which fail where it breaks what they say, and not a change to 2,500 lines at once.

## Consequences

- The capability checks can be read in a few hundred lines, and what they do for a number that is not a capability is stated, and is the same for every caller.
- A caller that passes a number that is not in the table gets `Invalid_Capability` where it got an undefined read or write. A program that depended on the old result will fail, and those are what the audit of the call sites is for.
- The proof rests on `Read_Slot`, `Write_Slot` and the heap allocator. They are small, but they are trusted.
- `Kernel_Process_Entry` changes: `Is_Active`, `Is_Cached`, `Last_Cap`, `Cap_Pages` and the cache move into the table. The snapshot and restore code that reads and writes them directly has to use `Restore` and the table's operations.
- The package is built for the kernel's run-time system, which has no `System.Assertions`, so its contracts are not executed (`Assertion_Policy (Ignore)`), as in the other packages that are proved.

## Alternatives considered

- **Keep the cache, and prove it agrees with the pages.** It needs the invariant that a cached layout is the page's contents unless it is newer, and that the cache is not full of the same capability twice, for a saving of one indexed read. It would be most of the work of the proof, for nothing that the kernel gets back.
- **A static pool of capability pages.** The pool and its allocation would be in SPARK too, and nothing here would be an address. It takes its memory up front, about a megabyte for 256 pages, and a process can then fail to get a page because of what the others have used. It can be done after this, if the two primitives are a problem.
- **Prove `Rose.Kernel.Processes` in place.** It uses addresses, the heap, the console and the debug output throughout, so nearly all of it would be outside SPARK.
- **Do #116 first.** It is a larger change, to code that nothing checks, and the table it is being added to is about to move.
- **Leave it.** The checks that Rose's security rests on would stay the part of the kernel that is least checked.

## Open questions

- What a slot holds for a derived capability, how many levels of derivation there can be, and whether validity is checked up the chain on every call or cached: answered in [ADR 0001](0001-capability-revocation.md), in the section on the derivation record (#116). A slot stays the layout, 16 bytes, with a 12-bit record id in it; there are at most 8 levels; and validity is checked up the chain on every call, with no cache.
- Whether the functions that take a process id stay for good, or whether the call sites move to the table in the process entry. Moving them is not needed for the proof.
- Where the table is saved at a checkpoint, once it has derivation records: with the records, in a checkpoint of `Kernel_Table_Version` 5 (ADR 0001).

## Follow-up work

1. Extract the table, with no change to what it does. The output is done by the functions that take a process id.
2. Remove the cache, and measure.
3. Put `SPARK_Mode` on, and prove P1 to P6, with a test of the package on a host, with a fake `Read_Slot` and `Write_Slot`.
4. Audit the call sites for a capability number that is not checked, and make each an explicit `Invalid_Capability`.
5. #116, on the proved table.
