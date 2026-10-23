# 1. Capability revocation

- **Status:** Proposed
- **Date:** 2026-10-22
- **Related:** #115 (remove the Create capability), Capabilities project

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
- #121: update `doc/rose-book/capabilities.tex` to describe rescind, derive and revocation.
