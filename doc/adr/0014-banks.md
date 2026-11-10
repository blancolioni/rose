# 14. Banks: where storage comes from, and how it is reclaimed

- **Status:** Accepted
- **Date:** 2026-10-29
- **Amended by:**
  - #448, where the store raises a signal of the server's for a dependent object, rather than invoking a cap that it registered, and registrations aren't saved: a restored server adds them again (sections 3 and 7)
  - #450, where init makes the system bank once, at the install, auth makes each user's bank, and the user server makes each session's (section 2)
- **Related:** [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0010](0010-resources.md) (resources), [ADR 0015](0015-process-ownership.md) (process ownership), #305 (allocate backing blocks on first write), #320 (reuse process table entries), #126 (the book's old Memory chapter)

## Context

Every process in Rose uses persistent storage, and today nothing limits or accounts for it. A process that ends has its own storage reclaimed, but nothing reclaims storage on a larger scale, such as everything belonging to a job or to a user.

### What exists today

- **The store hands out storage as regions.** `Storage.Reserve_Storage (Size)` returns a `Region`, which is a range of page object ids on the backing store. A region can create subregions, and `Region.Release` gives it back. The store keeps regions in a fixed table, and fails with "out of regions" when the table is full.
- **Elf reserves a process's storage when it launches it.** It reserves the stack, the heap and each private data segment from the store it is given, and adds them to the process as segments. Mem releases a process's private regions when the process ends.
- **Others reserve storage too.** The installer in init reserves storage for installed files. Servers that keep data for their clients reserve storage, or keep the data in their own memory.
- **Nothing limits a reservation.** Any holder of a `Storage` cap can reserve until the store is full, and storage that a server reserves for a client is charged to nobody.
- **Physical memory is a cache.** Mem keeps physical pages for the page objects that processes touch, and evicts or writes them back as needed. Nothing is charged for it.

### The idea so far

The book's old Memory chapter (#126) described hierarchical space banks. Each process belonged to a bank, a bank stood for a range of page object ids, a page object belonged directly to at most one bank, and a root bank spanned every id. Nothing implemented it, and the chapter was removed. ADR 0010 expects banks to make a client pay for a server's resources, so that reclaiming the client's bank reclaims them. ADR 0015 needs an owner for each process, so that ending a process ends what it launched.

### What's needed

- **Accounting.** Storage is charged to someone, and can be limited.
- **Reclaiming as a whole.** Everything a job, a session or a user reserved can be given back at once, without the cooperation of the processes that reserved it.
- **Charging the client, not the server.** A server that keeps something for a client can charge it to the client.
- **Isolation, and sharing when it's wanted.** One process can't use up another's storage, and two processes can share storage deliberately.

## Decision

### 1. A bank is a source of storage

- **A bank is an object that storage is reserved from.** `Bank` takes over `Reserve_Storage` from `Storage`. Every region is reserved from exactly one bank, and is charged to it.
- **The store serves banks,** because it already reserves and releases regions. A bank is not a range of page object ids: it records the regions reserved from it, wherever they are on the store.
- **The `Storage` cap that elf and the installer use today becomes a bank cap.**

### 2. Banks form a tree

- **A bank can create sub-banks.** Each bank has exactly one parent, except the root bank, which the store holds.
- **A sub-bank can have a limit,** in pages, and a limit on the number of regions. A reservation fails if it would take the bank, or any bank above it, over a limit. Storage reserved from a sub-bank counts against every bank above it. Pages count when the store has backing blocks for them (see 6), which today is when they are reserved.
- **The system's banks follow who is responsible.** Init receives a system bank at boot. The user server creates a bank for each user, and each session gets a sub-bank of its user's bank.

#### The system, users and sessions (amendment, #450)

Amended by #450. The tree is:

```
root (the store's)
└── system: init, at the install
    ├── exec, the command registry, and what init launches
    │   └── auth's own bank
    │       └── a user's bank: auth, for each user
    │           ├── the user's home directory and user server
    │           └── a session's bank: the user server, for each login
    │               └── petal, and the jobs it launches
    └── the banks that `.caps` files grant
```

- **Init makes the system bank once, at the install.** It is a sub-bank of the root bank, with no limits. Init is a boot module, and launches the persistent servers only at the install. They keep their caps across a restore, and a restored init launches nothing, so nothing needs to find the system bank again. Exec's own bank is the system bank, so a launch with a null bank lands there, not in the root bank. The update boot (ADR 0031) will need a way to find it again, to launch servers that replace old ones (#830).
- **Auth makes each user's bank,** not the user server. Auth is the process that creates users. A user's bank is a sub-bank of auth's own, and the user's home directory and user server are launched against it. The user server is given the bank, as an argument.
- **The user server makes each session's bank,** through `User.New_Session`, as a sub-bank of the user's bank. Login asks for one at each login, launches petal against it, and destroys it when petal returns. That ends whatever the session left running, and releases what it reserved, beyond what petal's own bank already covers (ADR 0015).
- **Ending auth ends every user.** The user banks are below auth's own bank, so destroying it would destroy them, with the users' home directories. That follows who is responsible: auth is what made them.
- **The limits are all unset.** Choosing defaults for a user or a session is a separate step, once usage can be read (`Get_Usage`).

### 3. Destroying a bank reclaims everything reserved from it

- **`Destroy` on a bank destroys its sub-banks, deepest first, and releases every region reserved from them.** The bank's caps then stop working, as caps of a released region do. Today a released region reports an empty range, because a handler can't fail a request yet (#230).
- **Nobody needs to cooperate.** The store does the releasing, so destroying a bank works whatever the processes that reserved from it are doing.
- **Objects can depend on a bank.** A server that creates an object on a client's behalf, such as mem with a process or a server with a slot for its client (ADR 0010), registers the object with the client's bank, giving a cap that the store invokes when the bank is destroyed. The server then removes the object. This is how a bank reclaims things that aren't storage, and how a client's bank pays for what a server keeps for it.

#### Dependent objects (amendment, #448)

Amended by #448. The store can't invoke a cap for each dependent object, for two reasons:

- **Messages would be lost.** The kernel queues one message for each process, and a second one sent before the first is received replaces it ("lost queued message"). A server with several objects under a destroyed bank, such as mem with every process of a session, would hear of one.
- **The store is on the pager path** (ADR 0008). It holds no caps to persistent processes, and saves its state as data, which can't hold a cap.

So a dependent object is told by a signal, as `SIGCHLD` is replaced in ADR 0006 (section 2): the server is told that something happened, and asks what.

- **`Bank.Add_Dependent (Signal)`** registers an object with a bank, and returns a cap for its `Dependent` interface, which the server keeps with the object. `Signal` is a signal cap for one of the server's own bits. It fails if the bank has been destroyed.
- **When a bank is destroyed,** the store marks the dependents of it and of its sub-banks as destroyed, and raises each one's signal. Raising a signal is a kernel operation that never waits, and signals merge, so none is lost and the store never waits for a server.
- **The server, when signalled,** asks `Destroyed` of the dependents it holds, removes the objects whose banks are gone, and releases their dependents.
- **`Release`** ends a registration, when the server removes the object for its own reasons too. `Dependent` is a `Resource` (ADR 0010).
- **A server that has ended** has no signal to raise: the raise fails, and the store drops the registration.

This settles the open question on a server that doesn't answer: the store never waits for one.

### 4. A process reserves from its own bank

- **Each process has its own bank,** a sub-bank of the bank that it was launched against (ADR 0015). Elf reserves the process's stack, heap and private data from it.
- **The process receives a cap for its bank when it starts,** with its other standard caps, and reserves any further storage from it.

### 5. Sharing

- **Storage is shared by sharing a region,** as now. The region is still charged to the bank it was reserved from.
- **Destroying that bank releases the region for everyone.** A process that relies on shared storage must not depend on the lifetime of a bank it doesn't trust. Storage that two processes share for longer should be reserved from a bank that outlives both, such as their common parent.

### 6. Physical memory isn't charged, for now

Physical memory remains a cache that mem manages for everyone. A bank's limit counts pages that have backing blocks on the store, not resident ones.

- **Today that is every reserved page,** because the store allocates a region's blocks when the region is reserved. A bank's page limit is then a guarantee: a bank within its limit can't fail to write, and the root bank can't be charged more than the partition holds.
- **If blocks are allocated on first write (#305), pages are charged when their blocks are allocated.** Reserving address space is cheap and can overcommit, as the sum of the banks' limits may be more than the partition. A write that would take a bank over its limit fails, and the failure is the fault of the bank that was charged. A region still counts against the region limit when it is reserved.
- **A process's writes don't reach a region's blocks yet.** Mem appends a process's dirty pages to the checkpoint log, and only a loader or installer writing a region's initial contents calls `Put`. So a process's stack costs no blocks and no bank pages. Blocks are allocated for a process's data when the log is consolidated into regions (#356), a long way from the write that dirtied the page, so the limit would not stop a process dirtying memory.
- **Charging reserved pages instead would not help #305.** The root bank's limit would be the partition, and the 34th 1 MiB stack would fail as it does now, though more cleanly.
- **`Get_Usage` can report reserved and committed pages separately,** once they differ.

### 7. Persistence

Banks, their limits and the regions charged to them are saved in the store's checkpoint. A destroyed bank stays destroyed after a restore.

Amended by #448: the registrations of dependent objects aren't saved. Each holds a signal cap of a persistent process, which the store can't save as data. A restored server adds its dependents again, when it is told that it has been restored (ADR 0004, ADR 0006). If a bank was destroyed in the meantime, `Add_Dependent` on its cap fails, and the server removes the object then.

## Consequences

- **Every caller of `Reserve_Storage` needs a bank cap:** elf, the installer, and any server that reserves storage.
- **Elf reserves from the process's bank,** not from a store that it was given once.
- **Store gains the bank table and the registration of dependent objects,** and saves both in its checkpoint.
- **Process ownership (ADR 0015) is built on this:** a process depends on its own bank, and ending it destroys that bank.
- **ADR 0010's `Release` stays.** A client that releases a resource gives it back early, and destroying the client's bank is the backstop for a client that doesn't.
- **Allocating blocks lazily (#305) fits, and moves the charge.** A bank's page limit counts pages with backing blocks (see 6). `Destroy` frees the blocks a region has, not a contiguous range. The page-to-block map and the banks go into the checkpoint in one format version, and a write that fails needs the store to report errors (#230).

## Alternatives considered

- **A bank is a range of page object ids,** as in the old book chapter. Every bank would need to know its size when it is created, a sub-bank would have to be carved out of its parent's range, and free space would fragment. Recording the regions reserved from a bank keeps the store's allocation as it is.
- **Mem serves banks.** Mem owns processes and physical memory, but it doesn't reserve or release storage, so releasing a bank's regions would take a call to the store for each one. Mem is one of the servers whose objects depend on a bank instead.
- **A separate bank server.** It would sit between every reservation and the store and add a hop, with no gain in authority, since the store has to trust it anyway.
- **Charging physical memory too.** It would bound a process's working set as well, but mem would need to charge every page fault to a bank, and to account for pages that several processes share. Counting pages with backing blocks is enough to stop one process using up the store.

## Open questions

- **Kernel objects.** Should process table entries, endpoints and caps be charged to a bank, so that a process can't use up the kernel's tables either?
- **The store's region table** is fixed in size. Should a bank's limit also count regions, not just pages?
- **Moving storage between banks,** for example handing a file to another user. Is it a new reservation and a copy, or can a region be transferred?
- **Reading a bank's usage,** for a shell command that reports what a job or a user is using.
- **Destroying a dependent object.** Settled by #448 (section 3): the store raises a signal and never waits.
- **Charging dirty memory.** Should mem tell the store when a page is first dirtied, with a call that allocates its block and charges its bank, and is a no-op once the block exists? A process over its limit would then fail at the write, in mem's fault path. The alternative is to charge when the log is consolidated (#356), where a failed `Put` leaves the data in the log and the limit doesn't stop a process dirtying memory.

## Follow-up work

- Store: a `Bank` interface with `Reserve_Storage`, `Create_Sub_Bank`, limits and `Destroy` (#447), and banks in the checkpoint (#518).
- Store: registration of dependent objects, and invoking them when a bank is destroyed (#448).
- Elf, the installer and other reservers: reserve from a bank (#449).
- Init: a system bank at boot. The user server: a bank for each user and each session (#450).
- The update boot: a way for init to find the system bank again (#830).
- ADR 0010: note that a server can charge a client's resource to the client's bank.
