# 32. Replacing a running persistent process

- **Status:** Proposed
- **Date:** 2026-11-11
- **Related:** #824 (this decision), #825 (userland cap sets), #831 (directory, auth and the user server), #830 (the update boot replaces servers), #116 (derivation records), #234 (four caps an invocation), [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0008](0008-install-boot-and-pager-path.md) (install boot and pager path), [ADR 0014](0014-banks.md) (banks), [ADR 0015](0015-process-ownership.md) (process ownership), [ADR 0021](0021-privilege.md) (privilege), [ADR 0031](0031-updating-an-installation.md) (updating an installation), #881 (saved state, section 10)

## Context

A persistent process is launched once and then restored, never relaunched (ADR 0008). Its code and its state are both in the checkpoint, so a new build doesn't reach it. ADR 0031 (section 5) lets an update boot replace a persistent process with its new version, if the server implements `Replaceable`, and leaves the details to this ADR.

### What exists today

- **A cap to a server names the server's process object id,** not its kernel slot. The kernel finds the process with `To_Process_Id`, and the endpoint with the endpoint id, from the IDL, and the server's capability identifier for the object. A client's caps go on working as long as some process answers that object id, endpoint id and identifier.
- **A process's storage is reserved from its own bank** (ADR 0015, #750). The store records which process owns each bank, by object id. When the process ends, mem asks the store to destroy its bank, which releases everything reserved from it, and ends the processes whose banks are below it. The directory server, for example, launches its file servers against its own bank, so ending the directory server ends them.
- **The kernel can already hold replies** while a checkpoint runs (`Checkpoint_Reply_Held`), and release them afterwards.
- **A server handles one call at a time,** in the `Rose.Server` loop. Between two calls, it is blocked receiving, and owes no work except replies it has deliberately kept: a console keeps a `Read` until a key arrives, and login waits for petal.
- **Cap sets** (#825) hold any number of caps, persist across a restore, and can be released. One cap carries a whole set, so #234's limit of four caps an invocation doesn't apply.

### What's needed

- The new version has to take over the old version's state, which only the program understands.
- Every client's caps have to reach the new version afterwards, with nothing for the client to do.
- Calls that arrive during the handover mustn't be lost or refused.
- If anything fails, the old version has to carry on as if nothing had happened.
- Only the update boot, and the install path, may replace a process.

## Decision

### 1. Who may replace a process

Replacing is a privileged operation, **`Memory.Replace_Process`**, whose cap only init's install path and the update boot's installer get, as the Create cap never leaves init.

- **It is mem's operation,** not one on the `Process` cap. Mem keeps the process table and the link between a process and its bank, both of which change. Mem does the kernel's part with a kernel operation that only mem holds (section 6).
- **It isn't given to owners.** In the update boot, the installer doesn't own the persistent servers, so ownership wouldn't let it replace them. A narrower right for an owner, such as auth replacing a user server, can be added later.
- **It isn't a role** that petal or `sudo` can grant (ADR 0021). Nothing in a running system replaces a process.

### 2. A server opts in with `Replaceable`

A server can be replaced only if its process implements the `Replaceable` interface. Mem tests for it, as features are tested, by interface (`in Replaceable'Class`), and refuses to replace a process that doesn't implement it. Exec and the command registry don't implement it, and nor do boot modules, which the update boot replaces with the boot set (ADR 0031, section 3).

`Replaceable` is answered by the old version, and called only by the new one:

- **`State return interface Reader`** gives the old version's state as a stream. It starts with a format version, and the rest is the server's own format. Ada's stream attributes, and the containers' stream support, do most of the work. A new version reads at least its predecessor's format.
- **`Caps return interface Cap_Set`** gives the caps that the old version holds, in a cap set (#825). The state has a section that names each cap and gives its index in the set, and the new version takes each one by name as it reads the state.
- **`Replies return interface Cap_Set`** gives the reply caps of the calls that the old version has deferred, in a cap set. The state lists each deferred call, with its index in the set (section 4).
- **`State` and `Caps` don't change the old version's state,** and may be called any number of times. If the handover fails, the old version goes on from where it was. `Replies` is the exception: a reply cap is moved, not copied, so it gives them out once, and raises if they are out already (#879).

### 3. The new version pulls what it needs

The new version is launched with the old version's `Replaceable` as an argument. It builds its own state from the state stream and the cap set, and takes only what it needs from them. Nothing is given to it that it doesn't ask for.

- **It gets a fresh bank of its own,** a sibling of the old version's: mem launches it against the old bank's parent.
- **It builds its storage in that bank,** from the state stream. A file server's contents, for example, come across in its stream, and the new version writes them into regions of its own.
- **The old version's own storage is released** when the old process ends, with its bank (section 6). The banks below the old bank, which belong to the processes it launched, aren't: they move to the new bank (section 6).
- **It accepts or refuses** by how its launch call ends. It completes if the new version has read the state and taken what it needs. It fails, by raising, if the new version can't read the format, or a cap it needs is missing.

The caps that the old version holds to other servers, and the callbacks its clients gave it, go in the cap set. The caps that clients hold to the old version's own objects don't: they move with the object id (section 6).

### 4. Calls during the handover

- **The old version must be between calls.** Mem waits until it is blocked receiving. If it is blocked in a call of its own, to another server, mem waits for that to finish, up to a limit, and then gives up (section 7).
- **The kernel holds new calls to the old version's object id,** as it holds replies during a checkpoint. The new version's calls to `Replaceable` are the exception: they are answered, or the handover could never finish.
- **Replies that the old version owes move to the new one.** A server defers a call with `Rose.Server.Block_Current_Request`, which keeps the call and its reply cap until `Unblock_Endpoint` answers it, as a console does with a pending `Read`. So the deferred calls move with nothing for the server to do (#879): the state lists each one, and `Replies` moves their reply caps into a set. The new version defers each call again, and answers it as the old version would have. A reply cap is moved, not copied, so it is still answered exactly once: the kernel moves every single-use cap that is sent, whether in a call, a reply or a new process's first caps, rather than copying it (#864). The sender's slot keeps its number but holds nothing, so a cap set server that gives a reply cap out a second time gives the null cap, and deleting the slot frees it.
  - **The reply caps stay in the set until the handover is decided.** The old version keeps a cap to the set, and whichever version next serves a call takes them out: the new version once it has taken over, since the held calls reach it then, or the old version, if the handover failed (section 7). Calls to `Replaceable`, and to the readers of the state, don't count, since the handover makes them. Neither version knows how the handover ended, but only the one that is serving calls can take the caps.
  - **Deleting a reply cap fails its call.** When a reply cap is deleted, the kernel wakes its caller with an error, if the caller is waiting for that reply, since a moved cap has no other copy (#879). So a set of reply caps that is released before either version takes them fails their calls, rather than leaving the callers blocked for good, as ending a process that holds a reply cap already does.
  - **A moved reply carries words and caps, but not the caller's buffer,** which the kernel mapped into the old version only. Answering into the buffer, as a console's `Read` does, is #878. Until then, a new version refuses to take over a deferred call that has a buffer, or caps.
- **The held calls are released to the new version** once it has taken over (section 6), or to the old one if the handover fails (section 7).

### 5. Checkpoints during a handover

Mem doesn't start a checkpoint while a handover is in progress, and a handover doesn't start while a checkpoint is running. A checkpoint therefore never holds a process half handed over, two processes with the same object id, or a bank tree half moved.

### 6. Taking over: the kernel moves the identity

When the new version's launch call completes, mem makes the switch:

1. **The kernel swaps the two processes' object ids, and gives the new version the old version's endpoint table.** The new version now answers the old version's object id, with each of the old version's endpoints at the same index, so that every client's cap names the same endpoint id and identifier as before. The kernel moves the new version's own caps to its endpoints, and its endpoints that the old version didn't have, to the indices they now have. The swap fails, with nothing changed, if the new version lacks one of the old version's endpoints. The old version takes the new one's launch-time object id, and is retired: calls to it fail until mem ends it. This is mem's kernel operation, and it is the only part that needs the kernel (#862).
2. **Mem swaps its own records** of the two processes, and calls the store's **`Process_Banks.Hand_Over (Old, New)`**. The store swaps the two banks' owners, so that each bank stays with the process that uses it, and moves every sub-bank of the old bank, with its subtree and its charges, under the new bank.
3. **The kernel releases the held calls,** which now reach the new version.
4. **Mem ends the old version** as it ends any process. Its bank, which now holds only the old version's own storage, is destroyed.

#### The processes the old version launched

The processes that the old version launched, such as the directory server's file servers, have their own banks below the old version's (ADR 0015). Destroying the old bank with them in it would end them, so `Hand_Over` moves them.

- **Every sub-bank moves,** not a chosen few. The children belong to the server's tree, not to its state, so no child ends because a new version forgot it. The new version gets their `Process` caps through the cap set, and can end one that it doesn't want by destroying its bank.
- **It is cheap.** The store changes the parent of each direct sub-bank, in its bank table, which the checkpoint saves (ADR 0014, section 7).
- **No limit can be broken.** The new bank is a sibling of the old one, so moving a subtree between them changes only those two banks' totals. Every bank above them keeps the same total.
- **The children don't notice.** Their own banks, owners and dependents (#448) are unchanged, and so are mem's records of them, which are keyed by their object ids.
- **It covers banks that aren't processes'.** Auth makes each user's bank a sub-bank of its own (#450), and those move too.

The new version's `Rose.Server` reattaches its objects to the identifiers it has been given, which the state records. The new version can also create endpoints that the old one didn't have, for operations that the new release adds (ADR 0031, section 2). A client finds them by asking for the newer interface, which descends from the old one, and keeps using the old one if it doesn't.

Caps that clients hold, including caps derived from the old version's (#116), name the object id, endpoint and identifier, not the process, so they reach the new version with nothing changed.

A derived cap whose parent is in the old version's cap table goes on working when the old version ends. Ending a process deletes its caps, and deleting a parent doesn't revoke what was derived from it: only rescinding does (ADR 0001). What the old version's end would lose is the power to revoke the children. So a server that may want to revoke what it handed out puts the parent in the cap set, as any other cap. A copy of a derived cap has the same derivation record, so the copy that the new version takes from the set can rescind the children, as the old version's could (#864).

### 7. Failure

Until step 1 of section 6, nothing has changed for the old version or its clients.

- **If the new version refuses, crashes, or doesn't finish in time,** mem ends it, which destroys its fresh bank, and releases the held calls to the old version. The new version hasn't served a call, so the reply caps of the deferred calls are still in their set, and the old version takes them back before it serves its next call (section 4).
- **If the old version doesn't become idle in time,** mem doesn't launch the new version, and reports why.
- **From step 1 on, the handover is committed.** Each later step is mem's own, or the store's, and can't be refused.

The updater reports a replacement that failed, and goes on to the next server (ADR 0031, section 5).

### 8. Tooling

- **`Rose.Server`** answers `Replaceable` for a server, and holds the server's identifiers and deferred calls, so a server writes only its state's stream attributes, and its list of named caps.
- **An IDL `Replaceable` aspect** on a server command (idle) generates the `Previous : in Replaceable` argument, and a main that reads the state if it is given one, and starts fresh if it isn't.

### 9. The first servers

The update test (ADR 0031, section 6) replaces `file`, whose open files have to keep working. Its contents come across in its state stream, so replacing it costs as much as the file. Then `directory`, `auth` and the user server (#831), whose state is mostly names and caps. Drivers implement `Replaceable` only if their device can be handed over safely: an interrupt or a device page can go in the cap set, but the device's own state can't be read back by the new version.

### 10. A saved state, of which a handover is a case

A server's state stream (section 2) is not only for a handover. A program can save it to a file, or send it over the network, and another instance of the program can start from it. A handover is one case of this: the state comes from a live predecessor, with its caps, and the new instance takes over its identity. Saving and restoring is the general case, with no caps and a new identity (#881).

- **`Saveable`** has `State`. **`Replaceable` descends from it** and adds `Caps`. A cap to `Saveable` lets its holder save the server, but not take its caps or replace it. Mem still tests for `in Replaceable'Class` (section 2).
- **A state can be saved at any time between calls.** A server handles one call at a time, so `State`, or a save that the server makes itself in one of its own handlers, always sees a whole state. As in a handover, saving changes nothing.
- **Caps don't go into a saved state.** A cap is an entry in a kernel table, not bytes. A saved state names the caps that the server held, but carries none of them.
- **A cap's name in the state is also a cap argument of the server's command.** A fresh launch gives the cap as that argument. A handover gives it from the cap set, and a restore from the arguments again. `Read_State` looks each cap up by name, and doesn't know where it came from. A missing cap fails a restore, as it fails a handover.
- **A restored instance is a new instance, not a replacement.** It has its own object id, so the old instance's clients don't reach it, and the old instance goes on running. The identifiers in the stream are kept: in a new process they name nothing that a client already holds, so reusing them is harmless.
- **The replies a server owes don't go into a saved state.** They can only be moved, in a live handover (section 4).
- **The format rule is the same,** but a saved file can be much older than a predecessor. A version reads every format of its series that a saved state may have.

`Rose.Replacement` gives a server both: **`Save`** writes the same stream as `State` to a `Writer`, and **`Restore`** reads one from a `Reader`, taking caps from the command's arguments. idle's `Replaceable` aspect also generates a **`Saved : in Reader`** argument, reserved as `Previous` is (section 8). The generated main takes over if it is given `Previous`, restores if it is given `Saved`, starts fresh if it is given neither, and refuses both.

Moving a server's clients to a restored copy, and carrying a state over the network, aren't part of this. Whatever carries the bytes is the program's business.

## Consequences

- **A 0.4.x can fix `file`, `directory`, `auth` and the user server** without losing the checkpoint, once each implements `Replaceable`.
- **0.4.0 has to ship the interfaces:** `Saveable`, `Replaceable`, `Memory.Replace_Process` and `Process_Banks.Hand_Over`. They freeze with the tag (ADR 0031).
- **The new version is a new program, not a patched one.** It has its own bank, cap table and layout, and drops what it no longer needs.
- **A handover costs the size of the server's state stream.** For most servers that is names and caps, but a server that holds data, such as `file`, streams all of it, and needs room for both copies until the old version ends.
- **The processes a server launched survive its replacement,** in the same bank tree, under the new version.
- **Each server owns its format,** and has to read its predecessor's for the whole series. Changing a format is a deliberate act, with a version word and a reader for the old one.
- **Clients don't notice,** except that calls during a handover take longer.
- **A replaceable server can also save its state** to a file or elsewhere, and a new instance can start from it, with caps given as arguments (section 10).
- **A server that never becomes idle,** because it is always in a call of its own, can't be replaced.

## Alternatives considered

- **The new version adopts the old bank.** It would keep the children's banks where they are, but it would also keep everything the old version reserved, including what it leaked, and the new version would inherit storage laid out by different code. Keeping only the children would need the store to release the bank's own regions and keep its sub-banks, which is `Hand_Over`'s move under another name.
- **Replicate the child banks in the new bank.** A child's storage is in its own process bank, below the parent's, so a replica would be an empty bank, and the child's code, stack, heap and data would still be charged to the old one. Replicating would work only if every child were ended and launched again, or its storage copied, which ends the directory server's file servers.
- **Move storage into the new bank** with a `Bank.Take` of each region the new version wants, rather than streaming it. A file server's handover would then cost nothing for its contents, but the new version would inherit regions laid out by the old code, and the store would need a way to move single regions between banks. It can be added if streaming a large server turns out to be too slow.
- **The old version pushes its state** to the new one. Pulling lets the new version ask only for what it understands, and lets the old version answer from its state without changing it.
- **Move the cap table in the kernel.** It needs no cap set, but it carries the old version's layout into the new one, and can't drop caps or tell when one is missing.
- **The owner replaces,** through the `Process` cap (ADR 0015). The update boot's installer doesn't own the persistent servers, so it would need every server's `Process` cap. It can be added for owners later.
- **Refuse calls during the handover.** Every client would need to retry.
- **Wait until no replies are owed.** Simpler, but a console or login always owes one, so neither could be replaced.
- **The new version re-creates its endpoints** with the old identifiers, and the kernel moves only the object id. The kernel would then have to check that the two tables match, and an endpoint the new version forgot would fail every client that used it.

## Open questions

- **Whether a server can be `Saveable` without being `Replaceable`.** It costs nothing in the interfaces, but the IDL aspect is `Replaceable` for now.
- **Reply caps with a buffer** (#878). A moved reply cap can't answer into the caller's buffer, which was mapped into the old version only. Should the kernel move the mapping, or should a server never defer a call that holds a buffer?
- **How long to wait** for the old version to become idle, and for the new version to finish. A fixed limit, or one the updater gives?

## Follow-up work

- Kernel: swap two processes' object ids and endpoint tables, and hold calls to an object id except on one endpoint.
- Mem: `Memory.Replace_Process`, and no checkpoint during a handover.
- Store: `Process_Banks.Hand_Over`, which swaps two banks' owners and moves one's sub-banks under the other.
- Kernel: move single-use caps, rather than copy them, when they are sent (#864). Derivation parents need nothing new: they go in the cap set as other caps do.
- Reply caps whose calls hold a buffer (#878).
- `Replies`, which hands the reply caps out once, and the kernel failing a call whose reply cap is deleted (#879).
- `Replaceable`, `Rose.Server`'s part of it, and the IDL aspect (idle).
- `Saveable`, `Rose.Replacement.Save` and `Restore`, and the `Saved` argument in idle (#881).
- `file` replaceable, and a test that replaces it on a restored system with files open.
- #831: `directory`, `auth` and the user server.
