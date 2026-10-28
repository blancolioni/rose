# 10. Resources: interfaces with a lifetime

- **Status:** Proposed
- **Date:** 2026-10-28
- **Related:** [ADR 0001](0001-capability-revocation.md) (capability revocation), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands), [ADR 0009](0009-files-open-readers.md) (files open readers), #389 (file readers), #390 (writers), #359 (finalization in the userland run-time system), blancolioni/idle#44 (the user target's servers)

## Context

Many of Rose's interfaces hand out something that a server has to keep until it is given back:

- **A file's readers** (ADR 0009). Each has a position in the file server's table.
- **The objects of a user-target server.** Each server package attaches up to 16 objects, and nothing detaches one. The kernel target's servers have `Remove_Instance`, which each server calls in its own way.
- **A pending timeout.** `Timer.Set_Timer` returns an `interface Cap` for it.
- **`Exec.Install`.** It returns a `Launch` interface for an installed program.
- **Cap sets,** from the `caps` constructor.
- **Memory, later,** when processes claim it from space banks.

Each of these ends in its own way, or not at all.

There is also the generated `Close` of every client, of both targets. It deletes the client's own caps, and nothing more. The server isn't told, so `Close` can't give back what the server holds for the client. A subprogram that does tell the server can't be called `Close` either, because the generated `Close` has the same profile.

## Decision

### 1. The `Resource` interface

```ada
interface Resource is
   procedure Release;
end Resource;
```

An interface whose objects hold something until it is given back inherits `Resource`. For example:

```ada
interface File_Reader : Stream_Reader, Resource is ...
```

- **`Release`** means "I'm done with this object: free what it holds". After it, every cap to the object fails, whoever holds it.
- **A program can release any `Resource`** without knowing what it is.
- **An interface without a lifetime doesn't inherit it.** `File`, `Directory` and `Stream_Writer` live as long as their server does, and a client's `Close` only deletes its caps, as now.

### 2. What idle generates

- **The client's `Close`** for an interface that is a `Resource` calls `Release`, then deletes the client's caps. A client that has not made a call yet, and so has no caps for the subprograms, still has its interface cap to release through.
- **On the user target,** once the run-time system has finalization (#359), a client of a `Resource` can be a controlled type whose `Finalize` closes it. An object then lives exactly as long as the client that reaches it, which is Ada's own notion of a lifetime. Until then, programs close their clients.
- **The user target's server** handles `Release` for every interface that is a `Resource`:
  - it calls the object's `Release` primitive, so the object can free what it holds;
  - it rescinds the object's endpoint caps (ADR 0001), so that every copy of them fails;
  - it frees the object's slot in the table, for another object to be attached.
- **The kernel target's servers** get a `Release` handler like any other subprogram, which calls `Remove_Instance` for the object as well.

### 3. Identifiers are not reused while caps to them might remain

A slot that is freed can be given to a new object. If the old object's caps were still valid, they would reach the new one. Rescinding them on `Release` prevents that. A server that cannot rescind, because it has no `Rescind_Cap`, does not reuse identifiers: it marks the slot as released, and leaves it empty.

## Consequences

- **The objects with a lifetime say so in their interfaces,** and end in one way. ADR 0009's `File_Reader` uses `Resource`, in place of a `Release` of its own. #390 decides whether writers do too.
- **Shared caps are released together.** If a client gives a copy of a resource's cap to another process, `Release` from either one ends it for both. That is deliberate: giving away a cap to a resource you will release is like granting it and then rescinding the grant. A program that wants to give away an object it doesn't control can derive a cap for it instead (ADR 0001, section 1).
- **A program that never releases still leaks.** `Release` relies on the client: one that forgets, or that is killed, leaves its objects in the server. Two larger mechanisms would cover that, and neither is part of this decision:
  - **a notice from the kernel when the last cap to an endpoint is deleted.** It must hold across checkpoints, since caps are persistent.
  - **space banks,** where the client's bank pays for the server's slot, and reclaiming the bank reclaims the resource.

  `Release` is compatible with both.
- **Existing interfaces are adopted one at a time.** Each one that inherits `Resource` changes the interfaces of its clients and servers:
  - file readers first (#389);
  - then the user-target server tables;
  - then `Timer`'s timeouts, installed programs and cap sets, as each is next worked on.
- **idle changes:**
  - `Close` calls `Release` for a `Resource`;
  - the user-target server handles `Release` as above;
  - an interface that declares a `Release` of its own, without inheriting `Resource` (`interfaces/resource.idl`), is reported, since its generated `Close` would not call it.
