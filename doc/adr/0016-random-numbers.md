# 16. Random numbers: an interface and a persistent service

- **Status:** Proposed
- **Date:** 2026-10-29
- **Related:** [ADR 0001](0001-capability-revocation.md) (capability revocation), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0007](0007-networking.md) (networking, the timer can lose expiries), [ADR 0010](0010-resources.md) (resources), [ADR 0020](0020-clock.md) (the clock), [ADR 0011](0011-end-of-file-at-the-console.md) (end of file), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (argument modes), [ADR 0013](0013-typed-streams.md) (typed streams), #419 (buffer types of `Stream_Reader`), #42 (SPARK)

## Context

Rose has nothing that produces random numbers. The first things that need them are the network (initial sequence numbers and, later, keys and nonces, ADR 0007), the capability servers (unguessable names, if any are ever wanted), and `Ada.Numerics` in the userland run-time system, whose `Reset` with no argument has to get its seed from somewhere. Tests want a generator that they can replace with a fixed one.

### What exists today

- **No entropy source.** Nothing in the kernel or the servers reads a random-number instruction, collects timings or keeps a pool.
- **Persistent processes.** A checkpoint holds a persistent process's memory and caps, and restore brings it back as it was (ADR 0004). A process that has to know it was restored implements `Restorable`, and the kernel sends it `Restored` before anything else it receives.
- **Caps are the only access.** A process can use a server only if it holds a cap to it, and a command gets its caps from its declaration (ADR 0002). There are no device files and no global names.
- **Streams carry bytes.** `Stream_Reader.Read` fills a buffer, and an empty read is the end of the stream (ADR 0011, ADR 0013).
- **Servers are single threaded,** and an invocation is a round trip. A server can't make a caller wait except by not replying (ADR 0002). The timer sends to a cap without `Block`, so an expiry is lost if the receiver isn't receiving (ADR 0007).

### What Linux does

`/dev/random` and `/dev/urandom` are two names for one generator on a modern kernel. It collects timings and device data in a pool, credits an estimate of the entropy that it holds, and seeds a ChaCha20 generator from the pool. A read blocks only until the generator has been seeded once. `getrandom(2)` is the same thing without a file, and writing to the device mixes data in without credit. A privileged ioctl (`RNDADDENTROPY`) mixes and credits. A virtual machine that is cloned or restored from a snapshot is told by a generation id so that it can reseed.

### The problem that is particular to Rose

Every unclean reboot in Rose is a restore of a snapshot. A generator whose key is in a checkpointed process goes back to the key that it had at the last checkpoint, and then produces again the bytes that it produced after that checkpoint. The processes that saw those bytes are rolled back with it, but the network isn't. A nonce, a sequence number or a key that left the machine would be used again.

The same applies to anything that is on the disk before the system runs: the install image boots the same way every time, and a copy of a disk starts two machines in the same state. A random service that treated its saved state as entropy would give them the same output.

### Constraints

- **Not a kernel service.** The kernel holds mechanism, and it has no policy for pools and credit (see Alternatives).
- **No heap, no generics.** The primitives have to build for both targets (ADR 0002 section 10). The ones below need neither.
- **Callers can be wrong about entropy.** A driver can claim more than it has.

## Decision

### 1. Three interfaces, three kinds of authority

Reading, mixing and crediting are different rights, so they are different caps.

```ada
interface Random : Stream_Reader is

   --  An endless stream of cryptographically secure bytes.
   --  Read fills the whole buffer, and never returns empty: an empty read
   --  means the end of a stream (ADR 0011), and this one has none.
   --  It fails with Not_Seeded until the generator has been seeded
   --  (section 3), and never waits.

   function Is_Seeded return Boolean;

end Random;

interface Entropy_Sink is

   --  Mixes data into the pool, without credit.  A caller can't make the
   --  output worse with it, so any process may hold a cap to this.

   procedure Add (Data : Stream_Element_Array);

end Entropy_Sink;

interface Entropy_Source : Entropy_Sink is

   --  For a driver that sees events that an outsider can't predict.
   --  Bits is the claim, and the service clamps it (section 4).

   procedure Add_Credited
     (Data : Stream_Element_Array;
      Bits : Word);

end Entropy_Source;
```

- **There is no ambient generator.** A process reads random bytes only if it was given a `Random` cap. A job's membrane can revoke or limit it (ADR 0001).
- **The buffer type follows #419.** The sketches use `Stream_Element_Array`, as `Stream_Writer` does.
- **`Not_Seeded` is an error,** reported the way servers report others.
- **`Read` is limited by the invocation's buffer.** The run-time system loops for more (section 7).
- **`Random` is a `Stream_Reader`,** so it can be a command's standard input, and a pipe or a tool that reads a stream can take it.

### 2. The generator and the pool

- **The pool is a BLAKE2s-256 accumulator** (RFC 7693) and a credit counter in bits, saturating at 256. Each `Add` hashes the source's identity, a counter, the length and the data, and a timestamp if the processor lets user mode read one.
- **The generator is ChaCha20** (RFC 8439), with a 256-bit key, and a generation number.
- **Each `Read` erases its key.** The first 32 bytes of the keystream become the next key, and the bytes after them are the output. A later look at the service's memory, or at a checkpoint, doesn't give back what was returned before it.
- **A reseed** replaces the key with BLAKE2s of the old key, the pool's digest and the generation number, then resets the pool and its credit.
- **No state outside the service.** There is no seed file. A checkpoint holds the service's memory, as it holds any process's.

### 3. Seeded and unseeded

- **The service starts unseeded.** It becomes seeded when it reseeds with 256 or more bits of credit.
- **Seeded means unpredictable,** not unique. The service never returns the same bytes twice, seeded or not (the key changes on every `Read`), but until it is seeded it doesn't claim that an outsider couldn't predict them, so it doesn't serve them.
- **After it is seeded, it reseeds from the pool at intervals,** whatever the credit, because more input can't hurt. The first interval is short and later ones are longer, and the service sets them with `Timer.Set_Timer`. Credit decides only whether the service is seeded, and only at the first seed and after a restore (section 5).
- **A client that has to wait** calls `Is_Seeded` again later. The service doesn't hold a request, because it is single threaded, and ADR 0007 says that a timer's notification can be lost.

### 4. Sources and credit

- **The service uses the processor's generator directly** when it has one: `RDSEED` or `RDRAND`, if CPUID reports it. User mode can execute these, so the service needs no cap for them. What it reads is mixed in and credited (see the open questions).
- **A driver holds an `Entropy_Source` cap,** which init grants at boot to the drivers that see unpredictable events: the timer (the tick against a cycle counter), the disk driver (completion times), the keyboard (key times) and, later, the network driver (packet arrivals). Each source's cap identifier names it, so a driver can't pass itself off as another and a cap can be revoked.
- **The service doesn't trust the claim.** It credits `Bits` up to the smaller of 8 per byte and a limit for that source, and it limits the credit that one source can add in a period. A timing source is credited at about one bit an event.
- **Data that an outsider can see is never credited.** What a driver read from the disk is public, and its timing may not be.
- **The checkpoint isn't a source.** The pool and key that come back from a checkpoint are mixed in, and never credited (section 5).

### 5. Restore

The service implements `Restorable` (ADR 0004). The kernel delivers `Restored` before the service handles any other message, so no `Read` can reach it first. The handler:

1. **Clears the seeded flag** and sets the credit to zero.
2. **Adds one to the generation number,** and replaces the key with BLAKE2s of the old key, the generation number and fresh input: the processor's generator, a cycle counter and the clock's two readings, `Monotonic` and `Realtime` (ADR 0020). The clock's own restore has run before it answers, so they are the restored clock's. Neither is credited: a clock can be set, and the time is public.
3. **Starts the pool again,** and sets the reseed timer again, since a timer isn't in the checkpoint's memory.
4. **Leaves the service unseeded,** unless the processor's generator has just given it enough credit. Until it is seeded again, `Read` fails with `Not_Seeded`.

What follows from it:

- **A restored machine never repeats output.** The key and the generation number have changed, and the bytes that it returned after the checkpoint are not returned again.
- **The disk doesn't hold the future output.** Someone who has the disk has the key at the checkpoint, and can compute what the service would have returned if it had continued. It doesn't continue: restore mixes in input that was never on the disk.
- **The disk doesn't hold the past output.** Each `Read` erased the key that made it.
- **The install image and a cloned disk** start the same way as a restore: unseeded, with nothing credited, and different from each other as soon as they take different input.

### 6. The service

- **A new server, `src/servers/random`,** laid out like `timer`. It is one persistent process with a fixed object id (#372). Persistence keeps the caps that other persistent processes hold to it valid across a restore, which a server that was started fresh at every boot would not.
- **init launches it early,** before the servers that use it, and gives it a cap to the timer and a `Clock` cap (ADR 0020), which the restore handler reads. Its entries are in the boot description and in the kernel's module lists, which the kernel checks against each other.
- **It publishes `Random`, `Entropy_Sink` and `Entropy_Source`** through `Server.Published_Interface`. init holds the cap that makes them, and it hands out each one separately: `Random` to the shell and to servers that ask for it, `Entropy_Sink` freely, `Entropy_Source` to the drivers in section 4 and no one else.
- **ChaCha20 and BLAKE2s** have no heap and no generics, so the same code builds for either target. Both have published test vectors, and both are good candidates for the SPARK work (#42).

### 7. Clients

- **A command asks for a generator in its declaration,** as it asks for any other cap (ADR 0002), and the shell binds the system's `Random` unless a user binds another. A test binds a fixed one, which is any server that implements `Random`.
- **The run-time system gets a `Rose.Random` package** over a `Random` cap: bytes, words, and a value in a range without modulo bias. `Ada.Numerics` `Reset` with no argument draws from it.
- **There is no generator in the client in the first version.** A client that cached bytes, or ran its own ChaCha, would be a persistent process with the problem in the Context: its cache goes back with its checkpoint. The first version has each request go to the service. A later version can add a client-side generator that implements `Restorable` and drops its state in `Restored`.

## Consequences

- **Rose gets a generator that is safe to restore.** That is the part that Linux added late, for virtual machines, and Rose needs from the start.
- **Reading never waits.** A command that needs bytes before the service is seeded gets an error, and has to try again.
- **Boot time may depend on entropy.** On a processor with no `RDRAND`, as a virtual machine may be, the service is seeded only when the timing sources have credited 256 bits. That has to take seconds, not minutes, and it is the first thing to measure.
- **A bug in a driver can't reduce the entropy,** because mixing can't make the pool worse. It can claim too much credit, and the service's limits are for that.
- **Every read is a round trip.** That is the cost of the first version, and the same as every other call to a server.
- **The kernel gets no new interface.**

## Alternatives considered

- **A generator in the kernel,** as Linux has. The kernel would hold a pool, a credit policy and a generator for every process to share. Rose's kernel has mechanism, not policy, and a kernel target without generics or a heap makes a pool harder to write than the same code in a server. The kernel would also be one more thing that a checkpoint has to treat specially.
- **A blocking pool with an entropy estimate for every read,** as `/dev/random` was. It stops processes without making them safer, and estimates of how much entropy a timing has are guesses. Modern Linux dropped it.
- **Fortuna,** with 32 pools and no credit. It is a good design with more parts, and the seeded flag here does most of the job. It could replace the pool later, behind the same interfaces.
- **A seed file instead of a persistent service,** as systemd does. In Rose the checkpoint is the persistence, so a file would be a second copy with the same replay problem, and it would need the file system before the first seed.
- **A service that is not persistent,** started fresh and unseeded at every boot. It has the same safety as a restore with fresh input, and it needs no `Restorable`, but a persistent client's cap to it would not survive a restore.
- **Credit as an argument of every `Add`.** That makes everything that holds a sink a possible source of overclaimed credit. A separate interface lets init decide who can claim.
- **One interface with a mode,** for read and write. Reading and adding are different rights, and ADR 0012 already uses the interface, not a mode, to carry a right.

## Open questions

- **Whether to trust the processor's generator.** The proposal credits `RDSEED` and `RDRAND` in full, as Linux does by default. The alternative is to mix and not credit, and to wait for the timing sources, which is safer against a processor that can't be trusted and slower to boot. A boot parameter could choose.
- **The credit for each timing source.** One bit an event is a guess. The numbers need to be measured on the hardware and under QEMU, with and without `-cpu` flags that report `RDRAND`.
- **Whether a user-mode cycle counter is available.** The design mixes one in if the kernel allows it (CR4.TSD), and has to do without if not.
- **Fail or degrade before the first seed.** The proposal fails with `Not_Seeded`. The alternative is to serve unique but predictable bytes, flagged as such, so that a boot never waits. Nothing needs randomness at boot today.
- **The reseed schedule.** The proposal is a short first interval and then longer ones, as Linux does, and the values are not chosen.
- **How a client waits.** `Is_Seeded` has to be polled. A `Notify_Seeded (Cap)` would work like `Timer.Set_Timer`, and has the same lost-notification problem (ADR 0007).
- **How large a `Read` can be.** The limit is the invocation's buffer, and the size isn't settled.
- **A client-side generator,** and how a client learns that it was restored. This is for a later version, and probably needs the run-time system to implement `Restorable`.
- **Who may hold `Entropy_Sink`.** The proposal is anyone. A membrane could remove it from a job that shouldn't be able to add.

## Follow-up work

- The three interfaces, and the generated clients and servers.
- The `random` server: pool, generator, seeding, credit limits and the `Restored` handler.
- ChaCha20 and BLAKE2s, with the RFC test vectors, and a proof plan under #42.
- The boot description, the object id, the kernel's module lists, and the caps that init grants.
- The feeds: the timer, the disk driver, the keyboard and, later, the network driver.
- `Rose.Random` in the run-time system, and `Ada.Numerics` `Reset`.
- A restore test: take a checkpoint, read, restore, and check that the bytes differ and that `Read` fails until the service is seeded.
- A measurement of the time to the first seed under QEMU.
- A section in the Rose Book.
