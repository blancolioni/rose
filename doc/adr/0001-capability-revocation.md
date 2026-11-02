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

The decision above says what a derived capability does. This says what the kernel keeps to do it, now that the capability table is a package of its own ([ADR 0023](0023-kernel-capability-table.md)) and its slot is private to it.

**What a slot holds.** A capability's layout is 128 bits, and every bit is used: the header is a type, three access flags, a 24-bit rescinded count, a 16-bit endpoint and a 16-bit identifier, and the payload is an object id. So the derivation is not in the layout. A slot of the table is the layout and a *derivation*: a record id, which is 0 for a capability that was not derived, and the count of that record that the capability was made with. The slot is 24 bytes, so a page holds 170, and a table has more pages (25) so that a process still has about 4,000 capabilities. A capability that is sent is its whole slot, copied. Nothing in an invocation changes: it still carries capability numbers.

**The record.** Derivation records are one table for the kernel, of 4,096. A record has its parent (a record id, or 0 for one that was derived from a capability that was not), the count of the parent when it was made, its own count, its depth, and the number of slots and child records that refer to it. The count of a record is never set back: when a record is freed and used again, its count goes on from where it was, so a capability from the record's last use does not match it (until the count has gone all the way round, which is after 2²⁴ rescinds of one record, as for a capability).

**Derive.** A new operation of the meta capability, `Derive_Cap`, takes a capability of the caller and gives the caller a new one: the same layout, and a new record whose parent is the record of the capability that it was made from. It does not give more authority than it takes, and the capabilities that narrow what they give (ports and interrupts now, and memory when it has a derive) make their narrower layout and then take a record in the same way. It fails with `Out_Of_Capabilities` when there is no record or no slot, and with `Invalid_Operation` when the depth would be more than 8.

**Valid.** A call through a capability is valid if:

1. its layout's count is that of the send capability of the endpoint, in the process that serves it: this is the check that is made now, and it is why rescinding the server's own capability revokes every copy, derived or not;
2. and, if it has a record, the count that it was made with is the count of its record, the record's recorded count of its parent is the count of the parent, and so on up to a record with no parent.

That is at most nine reads, and no allocation. A depth of 8 is what bounds it, so there is no cache of validity: a cache would need what it caches to be revoked, which is the problem.

**Rescind.** Rescinding the server's own capability is what it is now. Rescinding a derived capability adds one to the count of its record, which makes every copy of it invalid, and every capability derived from it (their records record the old count of their parent), and nothing else: not the parent, and not the siblings. The capability that was rescinded stays valid: its slot has the new count, as the server's own does.

**Lifetime.** A record is freed when nothing refers to it. A slot refers to its record, and a record to its parent. A copy of a slot adds one, and so do `Derive_Cap` and a restore; `Delete_Cap`, a single-use capability that is used, and the death of a process, which will walk the capabilities it has, take one away. A record that has none left is freed, and takes one from its parent, which may be freed in turn. A record is not freed when the capability that it was made for is deleted, since the copies of it that were sent are valid, and are what it is for.

**Checkpoints.** A checkpoint writes the records with the capability tables (#117), and a capability that is saved has its record and its count. `Kernel_Table_Version` becomes 5. A restore rebuilds what refers to what from the slots and the records, and does not trust the numbers that were saved: a table in which a capability names a record that is not in use, or a record's depth is not its parent's and one, is not restored.

**What is proved.** The records and the slots' use of them are a SPARK package, as the table is. What it is proved to keep: the depth of a record is its parent's and one, and at most 8, so there are no cycles; a record that is free is not named; the number of references to a record is the number of slots and records that name it; and, of a rescind, that it makes invalid the capabilities of the record and of every record below it, and no other.

**Why not another way.**

- *A larger layout.* It changes every user of the layout (the invocation, the checkpoint, the debugging output) to put eight bytes in a place that only the table reads.
- *Bits taken from the layout.* Twelve bits would have to come from the count, the endpoint index or the identifier. A count of 16 bits is wrapped after 65,536 rescinds, which a server that revokes a client at a time reaches; the endpoint index and the identifier are 16 bits each, and cutting either puts a limit on every server.
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
