# 1. Capability revocation

- **Status:** Proposed
- **Date:** 2026-10-22
- **Related:** #115 (remove the Create capability), Capabilities project, [ADR 0023](0023-kernel-capability-table.md) (the kernel capability table)
- **Amended by:** #116, which settles the derivation record (in section 1)

## Context

Rose needs a way to take back capabilities it has handed out. Today it has one mechanism: **rescind**.

- An endpoint cap carries a rescinded count. The serving process holds its own endpoint cap for that endpoint (`Endpoints (I).Send_Cap` in the kernel process table).
- Invoking the meta cap's `Rescind_Cap` endpoint bumps the count on a cap in the caller's own table.
- On every endpoint invocation, `Rose.Kernel.Processes.Is_Valid_Entry` compares the invoked cap's count with the server's endpoint cap. If they differ, the call fails with `Invalid_Capability`.

So rescinding the server's endpoint cap invalidates **every copy of that endpoint**, held by any client. This gives us two gaps:

1. **No per-grant revocation.** A server can't cut off one client while keeping the others. Each 64-bit endpoint id maps to a single local endpoint index, so the only granularity is the whole endpoint.
2. **No transitive revocation.** If process A gives B a cap, and B uses it to obtain further caps (for example from a factory or directory), revoking A's grant does not reach the caps B obtained through it.

Other constraints:

- **Servers are single-threaded** (`pragma Restrictions (No_Tasking)`), so anything that interposes on calls is a serialization point.
- **Memory is not mediated by invocation.** Once a page object is mapped, or an invocation buffer is shared, access goes straight to the memory, with no call to intercept.
- **Hardware caps** (port I/O, interrupts, physical memory) have kernel-level semantics that a user-space process can't usefully forward.
- **The system is persistent.** Object ids are stable across restores, and the kernel's capability tables are written to the log at each checkpoint. Revocation state must survive a restore.
- **Derive is planned (#115).** Removing Create introduces a derive operation that produces narrower caps from root caps.

### Option considered: membranes

A membrane is a forwarding process that wraps a cap, transitively wraps every cap passing through it in either direction, and can be revoked as a whole. It is the only option that solves gap 2. As Rose's *only* mechanism, it falls short:

- **Cost:** every call through a membrane is two IPCs instead of one, plus extra context switches. Each wrapped cap also needs an endpoint in the membrane process, and endpoint tables are limited (`Rose.Server` allows 80 instances).
- **Blocking:** a single-threaded membrane that blocks on one forwarded call stalls every other call through it.
- **Memory:** it can't revoke mapped pages or shared buffers.
- **Hardware caps:** forwarding interrupts isn't meaningful, and forwarding port I/O is too slow for drivers.
- **Identity:** the same cap wrapped twice gives two distinct caps, and caps travelling back to their origin have to be unwrapped. This needs careful bookkeeping in a user-space process.

## Decision

Revocation has three layers, each used where it fits.

### 1. Kernel: per-grant revocation through derived caps

- **Derived caps get their own rescinded count.** Every cap created by the derive operation (#115) carries its own count, recorded in a kernel derivation record that names its parent.
- **Validity checks the whole chain.** A cap is valid only if its own count and every ancestor's count match. Rescinding a derived cap revokes that grant and everything derived from it. The parent and its other children are unaffected.
- **Derive also works on endpoint caps.** A server gives each client its own derived cap, so it can revoke one client at a time.
- **Existing rescind stays.** Rescinding the server's own endpoint cap still revokes every copy, as it does today.

This follows the approach of seL4, which records a derivation tree and revokes the subtree under a cap, and of KeyKOS/EROS, which used kernel version counts and forwarders in a persistent capability system.

#### The derivation record (amendment, #116)

The decision above says what a derived capability does. This says what the kernel keeps to do it, now that the capability table is a package of its own ([ADR 0023](0023-kernel-capability-table.md)).

**Where the record id goes.** A capability's layout is 128 bits, and every bit is in a field, but not every field is needed: the endpoint index is 16 bits, and the largest in the kernel is 31 and a process registers at most 32; the rescinded count is 24 bits, and 20 bits wrap after a million rescinds of a server's own capability (per-grant revocation is the record's, so a server no longer rescinds its own capability to revoke one client). So the header is repacked: the endpoint index is 8 bits, the rescinded count 20, and a 12-bit *record id*, which is 0 for a capability that was not derived, takes the 12. Nothing else is wider: a slot of the table is the layout, 16 bytes, a page holds 256 of them, and a process has the same 4,095 capabilities. A capability that is sent is its layout, copied, and so is its record id. Nothing in an invocation changes: it carries capability numbers. The header of the layout is a type, three access flags, the count, the endpoint, the identifier and the record id, as the book's layout of it says, and `make check-layouts` checks.

**The record.** Derivation records are one table for the kernel, of 4,096 (what 12 bits name). A record has its parent (a record id, or 0), its depth, whether it has been revoked, and the number of slots and child records that refer to it. It has no count: a record is not freed while anything names it, so no capability can name one that has been freed and used again, and revoking is a flag and not a number to compare.

**Derive.** A new operation of the meta capability, `Derive_Cap`, takes a capability of the caller and gives the caller a new one: the same layout, and a new record whose parent is the record of the capability that it was made from. It does not give more authority than it takes, and the capabilities that narrow what they give (ports and interrupts now, and memory when it has a derive) make their narrower layout and then take a record in the same way. It fails with `Out_Of_Capabilities` when there is no record or no slot, and with `Invalid_Operation` when the depth would be more than 8.

**Valid.** A call through a capability is valid if:

1. its layout's count is that of the send capability of the endpoint, in the process that serves it: this is the check that is made now, and it is why rescinding the server's own capability revokes every copy, derived or not;
2. and, if it has a record, that record and each of its ancestors has not been revoked.

That is at most nine reads, and no allocation. A depth of 8 is what bounds it, so there is no cache of validity: a cache would need what it caches to be revoked, which is the problem.

**Rescind.** Rescinding the server's own capability is what it is now. Rescinding a derived capability revokes its record, and gives the capability that was rescinded a new one, with the same parent: every copy of it is invalid, and every capability derived from it (they name a record whose ancestor is revoked), and nothing else: not the parent, and not the siblings. The capability that was rescinded stays valid, with the new record. If there is no record to give it, it is revoked with the rest, and its holder derives another.

**Lifetime.** A record is freed when nothing refers to it. A slot refers to its record, and a record to its parent. A copy of a slot adds one, and so do `Derive_Cap` and a restore; `Delete_Cap`, a single-use capability that is used, and the death of a process, which will walk the capabilities it has, take one away. A record that has none left is freed, and takes one from its parent, which may be freed in turn. A revoked record stays until its copies are deleted, which is when it has none. A record is not freed when the capability that it was made for is deleted, since the copies of it that were sent are valid, and are what it is for.

**Checkpoints.** A layout is saved as its 16 bytes, so a capability is saved with its record id, and the checkpoint also has the records (#117). `Kernel_Table_Version` becomes 5. A restore rebuilds the counts of references from the slots and the records, and does not trust the numbers that were saved: a table in which a capability names a record that is not in use, or a record's depth is not its parent's and one, is not restored.

**What is proved.** The records and the slots' use of them are a SPARK package, as the table is, and the layout's new fields are in the proved `Rose.Capabilities.Layout`. What the records are proved to keep: the depth of a record is its parent's and one, and at most 8, so there are no cycles; a record that is free is not named; the number of references to a record is the number of slots and records that name it; and, of a rescind, that it makes invalid the capabilities of the record and of every record below it, and no other.

**Why not another way.**

- *A larger slot, with a count for each record.* A slot of 24 bytes would hold a record id and the count of the record that the capability was made with. The count is not needed: reference counts mean that no capability names a record that has been freed, so a flag on the record, and a new record for the handle that was rescinded, do what the count did, in 12 bits and not 36, and without changing a page, the limit of a process, or the checkpoint's capability.
- *A larger layout.* It changes every user of the layout (the invocation, the checkpoint, the debugging output) to make room that the header has.
- *Records for each process.* A grant is derived in one process and used in others, so its record is not any one process's.
- *Free a record at a checkpoint, by finding which are named.* It is the reference count done once in a while, and a record would stay until the next one.

### 2. User space: membranes for sandboxing

- **A membrane library** gives transitive revocation where it's actually needed, built on top of the kernel primitive.
- **Petal is the first user.** It wraps the caps handed to a command when the command launches, and revokes the membrane when the command exits or is killed.
- **Membranes don't forward everything.** They forward only endpoint, interface and reply caps. Memory and hardware caps are never passed through a membrane. Their holders get derived caps instead, so they can still be revoked under layer 1.

### 3. Memory server: revoking memory

- **The memory server revokes memory.** Taking back a page object or region unmaps it from every process that has it mapped. The kernel is responsible for invocation buffers.
- **Memory caps come from derive.** Page and region caps handed to other processes are derived caps, so layer 1 controls who can map them, and layer 3 removes mappings that already exist.

## Consequences

**Benefits**
- A server can revoke one client without affecting the others.
- Revocation is cheap in the common case: one count check per ancestor on each invocation, and no extra IPC.
- Transitive revocation is available where it matters (sandboxed commands) without making every call pay for it.
- Revocation survives restore: rescinded counts and derivation records live in the kernel tables that each checkpoint saves, and membranes are ordinary persistent processes.
- It fits #115, so derive is designed once and serves both narrowing and revocation.

**Costs**
- **Kernel state:** each derived cap needs a kernel derivation record. These have to be saved in checkpoints along with the capability tables, and the checkpoint format has to include them.
- **Chain depth:** invocation cost grows with derivation depth. The kernel may need to cap the depth or cache validity.
- **Membrane library work:** it has to handle unwrapping, cap identity, and not blocking (for example, one membrane per sandboxed command, so one stuck call only stalls its own command).
- **Two paths for memory:** revoking memory needs both a rescind and an unmap. These have to happen in a defined order, so the cap is never invalid while the mapping is still live.

## Follow-up work

- #116: add per-grant counts and derivation records to the derive operation in #115.
- #117: the kernel writes its tables to the log at checkpoint start, including derivation records.
- #118: membrane library.
- #119: petal runs commands inside a membrane.
- #120: memory server revocation: unmap a revoked page or region from every process that has it mapped.
- #121: update `doc/rose-book/capabilities.adoc` to describe rescind, derive and revocation.
