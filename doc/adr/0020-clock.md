# 20. The clock: monotonic and real time for user mode

- **Status:** Proposed
- **Date:** 2026-10-30
- **Related:** #494 (this decision), [ADR 0001](0001-capability-revocation.md) (capability revocation), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0007](0007-networking.md) (networking, time after a restore), [ADR 0008](0008-install-boot-and-pager-path.md) (the pager path uses the kernel's tick caps), [ADR 0016](0016-random-numbers.md) (random numbers, which mixes in the clock), #372 (fixed object ids), #497 (`Ada.Calendar`), #363 (tasking, `Ada.Real_Time`), #193 (the tick, and a finer clock), #42 (SPARK)

## Context

User-mode code can't read a clock. `Ada.Calendar` can't be built (`No_Calendar` is set in the userland run-time system), files and checkpoints carry no timestamps, and a restored network stack can't tell how long the system was down (ADR 0007). The only time that Rose has is the kernel's tick counter.

### What exists today

- **A tick counter.** `boot.s` programs the PIT with a divisor of 11932, so a tick is 11932 / 1193181.8 s, which is 10.00015 ms, and `Rose.Kernel.Clock` counts them in a 32-bit word that wraps after about 16 months. It starts at zero at every boot.
- **Caps for it.** `Get_Current_Ticks` and `Set_Timeout` are kernel caps that only init holds. `timer` and `ata` get them from init, and the pager path uses them directly (ADR 0008). `Timer.Set_Timer` counts a tick as 10 ms.
- **No wall clock.** Nothing reads the CMOS real-time clock (RTC).
- **A restore is the normal reboot.** Every unclean boot restores a checkpoint, so whatever time a process saw before the checkpoint is a time that the restored process still remembers, and the machine has run, or been off, for some time since (ADR 0004).
- **The RTC can be wrong.** A flat battery, a fresh virtual machine and a copied disk all give an RTC that is far from the real time, usually early.

### Constraints

- **Not a kernel policy.** The kernel has the counter. It doesn't own a time of day, and it shouldn't know about an RTC.
- **Caps are the only access** (ADR 0002). Reading the time and setting it are different rights.
- **Both targets** build the servers. Only i686 is a target for the kernel for now, and port I/O in a server goes through a port cap from init.

## Decision

### 1. Two interfaces, two kinds of authority

```ada
interface Clock is

   --  Nanoseconds since the system was first started, not counting the
   --  time that it was off. Never goes backwards, including across a
   --  restore (section 4).

   function Monotonic return Word_64;

   --  Nanoseconds since 1970-01-01 00:00:00 UTC, without leap seconds
   --  (every day has 86400 seconds). Can jump, forwards or backwards,
   --  when someone sets it, and only then goes backwards (section 4).

   function Realtime return Word_64;

   --  False when Realtime is known to be a guess: the RTC's battery bit
   --  is clear, its fields are out of range, or the clock is held at the
   --  checkpoint's time (section 4). True again after Set_Realtime.

   function Realtime_Is_Trusted return Boolean;

end Clock;

interface Clock_Admin is

   --  Sets the real time, and writes it to the RTC.
   --  Fails if the RTC can't hold the time (before 2000 or after 2099).

   procedure Set_Realtime (Nanoseconds : Word_64);

end Clock_Admin;
```

- **Two readings, because they answer two questions.** `Monotonic` measures how long something took, and is what timeouts, `Ada.Real_Time` and retransmission timers want. `Realtime` says what the time is, and is what `Ada.Calendar`, file timestamps and lease lifetimes want. A caller that wants an interval must not use `Realtime`, since someone can set it.
- **`Clock` can be given to anyone.** It is read only. A job's membrane can still remove it (ADR 0001), which also removes a way to measure timings.
- **`Clock_Admin` is a separate cap.** init holds it, and hands it only to a service or a command that may set the time. A failing `Set_Realtime` fails by raising, as the other operations do.
- **A `Word_64` is enough.** In nanoseconds it reaches the year 2554 for `Realtime`, and 584 years of uptime for `Monotonic`.
- **The resolution is one tick,** 10 ms. A reading is quantised to a tick. A finer clock (the TSC, calibrated against the PIT) can be added behind the same interfaces later.
- **The tick is 10 000 150 ns.** The server converts with the real period, not 10 ms, because 10 ms would lose 15 parts per million, which is more than a second a day.
- **No time zone and no leap seconds.** The RTC holds UTC. A zone is a user setting, and belongs to whatever has settings. Until there is one, the run-time system's `Ada.Calendar.Time_Zones` reports zero.

### 2. A persistent `clock` server

- **A new server, `src/servers/clock`,** laid out like `timer`. It is one persistent process with a fixed object id (#372), so that the caps that other persistent processes hold to it stay valid across a restore.
- **It is not in the kernel.** The kernel keeps its counter and gets no time of day, no RTC driver and no offset to checkpoint.
- **It holds three things:** the kernel's get-ticks cap, a port cap for the RTC's two ports (`70` and `71`), derived by init the way other drivers get theirs, and its own state (section 3).
- **init launches it early,** before the servers that use it, and publishes `Clock` and `Clock_Admin` through `Server.Published_Interface`. Its entries are in the boot description and in the kernel's module lists.
- **A read is a round trip.** That is the cost of the first version. A page that the kernel shares read-only, which a client reads without a call, is a later option, and needs the offsets to be there.

### 3. How a reading is made

The server keeps, in its own memory, the values that make a reading from the tick counter:

- `Base_Ticks` and `Base_Mono`: the tick count and the monotonic time at one moment, so that `Monotonic = Base_Mono + (Ticks − Base_Ticks) × 10 000 150`.
- `Realtime_Offset`: so that `Realtime = Monotonic + Realtime_Offset`.
- `Last_Mono` and `Last_Real`: the latest readings that it returned.
- **The RTC is read only to set the offset.** It has a resolution of one second, so it is read at the start, at a restore and never again (see Consequences for drift).
- **The read finds the edge of a second.** The server polls the RTC until its seconds field changes, and takes the tick counter at that moment. The offset is then accurate to a tick, not to a second. The wait is up to a second, once at the start and once at each restore. The same helper serves `Set_Realtime` (section 5).
- **Reading the RTC** waits for the update-in-progress flag to clear, reads the registers twice until two reads agree, and follows the status register for BCD or binary and for 12 or 24 hours. The year is taken as 20yy, because the century register is not reliable. The NMI bit of port `70` is left alone. Register D's valid bit and the range of the fields decide whether the reading can be trusted.
- **The counter is widened.** The kernel's tick counter is 32 bits and wraps. The kernel gets a 64-bit counter, and `Get_Current_Ticks` is replaced by `Get_Current_Ticks_64`, which replies with the low and high words. The old endpoint and its constants are removed, not kept, so that `timer`, `ata` and the pager path fail to build until each is moved over, and none reads half a counter by mistake. This is the only kernel change. The alternative, in the server, is to notice wraps, and that needs something that calls it at least every 16 months.

### 4. Restore

The server implements `Restorable` (ADR 0004). The kernel delivers `Restored` before the server handles any other message, so no reading can use the old tick values first. The tick counter restarts at zero after a reboot, so the old `Base_Ticks` means nothing. The handler:

1. **Takes a new base for ticks:** `Base_Ticks` is the counter now, and `Base_Mono` is `Last_Mono`. Monotonic time continues from the last value that the server returned.
2. **Reads the RTC,** at the edge of a second (section 3).
3. **Sets the offset so that `Realtime` is the later of the RTC and `Last_Real`.**
4. **Records whether the RTC was earlier,** in which case the clock is held at the checkpoint's time, and runs from there. That is a clock that is known to be wrong, and `Realtime_Is_Trusted` is False until someone calls `Set_Realtime`. It is also False if the RTC's valid bit was clear or its fields were out of range.

What follows from it:

- **Monotonic time never goes backwards.** Everything that a restored process holds was a reading from before the checkpoint, and `Last_Mono` is at least as late, because the server's memory and its clients' are from the same checkpoint. Monotonic time doesn't count the time that the machine was off, so an interval that spans a restore is shorter than the real one, and not negative.
- **Real time doesn't go backwards at a restore** and includes the downtime, so a DHCP lease or a DNS answer can be aged across a restore, which ADR 0007 asked for.
- **A bad RTC makes the clock stop, not go back.** After a restore with an early RTC, readings continue from the checkpoint's time, behind the real time by the length of the downtime.
- **A first boot** has no `Last_Real`, so the RTC is taken as it is. An install on a machine with a dead RTC starts in 2000, with `Realtime_Is_Trusted` False if the valid bit says so, until someone sets it.
- **Only `Set_Realtime` moves real time backwards,** because that is what it was asked to do. It also sets `Last_Real`, and makes `Realtime_Is_Trusted` True.

### 5. Setting the time

`Set_Realtime`:

1. **Checks the range** the RTC can hold, and fails before changing anything.
2. **Sets `Realtime_Offset`** so that the reading is the new time, and sets `Last_Real` to it.
3. **Writes the RTC at a second boundary,** in UTC. The RTC has whole seconds, so the server computes the time of the next whole second, waits for the clock to reach it (up to a second, with the server single threaded for that time), then sets the SET bit in register B, writes the date and time registers in the format the status register describes, and clears SET. The RTC and the clock then agree to a tick, and a restore doesn't lose up to a second.

### 6. Clients

- **The run-time system gets a `Rose.Clock` package** over a `Clock` cap: `Monotonic` and `Realtime` as `Word_64`, and conversions.
- **`Ada.Calendar` is built on it** (#497), and `No_Calendar` goes from the userland restrictions. `Ada.Calendar.Clock` is `Realtime` converted to `Time`, and the zone is UTC (section 1). It returns the reading when `Realtime_Is_Trusted` is False, since it can't fail. `Ada.Real_Time` and `delay until` can be built on `Monotonic` with the tasking work (#363).
- **A command asks for a clock in its declaration,** as it asks for any other cap (ADR 0002), and the shell binds the system's `Clock` unless a user binds another. A test binds a fixed one, which is any server that implements `Clock`.
- **A server that needs timestamps** (`checkpoint`, a file server) takes a `Clock` cap from init, as the random server will.
- **`timer` doesn't change.** It still counts in ticks. A later change could let it take a `Monotonic` deadline.

## Consequences

- **Rose has a clock that survives a restore without going backwards.** That is the part that is particular to Rose, since a restore is the normal boot.
- **Monotonic and real time can differ after a restore.** An interval measured with `Monotonic` doesn't include the downtime, and one measured with `Realtime` does. That is what each is for, and the documentation has to say so.
- **The real time drifts.** After the start it is the tick counter, which is as good as the PIT's crystal, and is not corrected. A day can be off by a second or more on hardware. Correcting the clock against the RTC, or against the network, has to slew and not step, and is for later.
- **The time is accurate to a tick,** 10 ms, at the start and after, until it drifts.
- **Boot and restore wait for up to a second** for the RTC's edge. That has to be measured, and if it matters the wait can move to a background step, with `Realtime_Is_Trusted` False until it is done.
- **A wrong RTC is a clock that is early or stopped,** and `Realtime_Is_Trusted` says so when the server can tell. A dead battery that still reports a valid bit can't be told from a correct one.
- **A call to `Set_Realtime` blocks the server for up to a second,** which is acceptable for something that is called rarely, and means that readings wait.
- **The kernel changes by one endpoint.** `Get_Current_Ticks` is replaced, and its three callers change with it. There is no RTC code in the kernel.
- **Every reading is a round trip,** as every call to a server is.
- **ADR 0016's restore handler** mixes in "the real-time clock". It should mix in `Clock`'s two readings instead, and the random server should be given a `Clock` cap. That ADR is amended separately.

## Alternatives considered

- **The kernel keeps real time,** and a kernel call reads it. A call is faster than a round trip, and it needs no server. The kernel would then own an RTC driver, an offset and a policy for restore, and the offset would be one more thing that a checkpoint treats specially. The kernel would do for the clock what ADR 0016 declines to do for random numbers.
- **One interface with a mode,** for reading and setting. Reading and setting are different rights, and ADR 0012 already uses the interface, not a mode, to carry a right.
- **One reading.** `Ada.Calendar` alone needs only real time, but a timeout built on a clock that someone can set fails when they set it, and Ada's two packages need two clocks.
- **Monotonic time that counts the downtime.** It would need the RTC at every restore, and it is as wrong as the RTC. A restore would also make every timeout that a process held in a checkpoint expire at once.
- **Restart monotonic time at zero at each boot,** as Linux does. A persistent process can hold a reading from before a restore, and then see time go backwards. Here the persistent processes are the norm.
- **Read the RTC on every call.** It costs port I/O and a wait for the update flag, and has a resolution of a second.
- **A separate time-zone-aware clock.** A zone is a setting for a user, not a property of the machine.
- **Ada's epoch (1901) in the interface.** The interface shouldn't carry a language's choice, and a 1970 epoch needs no negative numbers.
- **Seconds and nanoseconds as two words,** for `Realtime`. The IDL has `Word_64`.

## Open questions

- **What a client does when `Realtime_Is_Trusted` is False.** `Ada.Calendar.Clock` can't fail, so it returns the reading, and a command that cares (a file server stamping files, say) can ask. Whether the shell should warn the user at login is not decided.
- **How long the edge wait takes at boot and at restore,** and whether it should move off the boot path. This needs measuring under QEMU.
- **How a time zone is set,** and where it is kept, when something other than UTC is wanted.
- **Whether a program that sets the time needs more than the cap,** for example a record in the login's log of who did it.

## Follow-up work

- The two interfaces, and the generated clients and servers.
- The `clock` server: the RTC, the offset, `Restored` and `Set_Realtime`.
- The boot description, the object id, the kernel's module lists, and the caps that init grants (ticks and ports).
- The kernel's 64-bit counter and `Get_Current_Ticks_64`, replacing `Get_Current_Ticks`, with `timer`, `ata` and the pager path moved over.
- `Rose.Clock` in the run-time system. `Ada.Calendar` and removing `No_Calendar` from the userland restrictions are #497, which is blocked on this.
- A test: read the clock twice with a delay between, check that `Monotonic` advanced and that `Realtime` is within a second of the host's, and cover a restore (a checkpoint, a delay, a restore, and a check that neither reading went backwards and that real time includes the downtime).
- A test for `Set_Realtime`, including that the RTC has the new time after a reboot.
- A proof plan under #42: the conversions from RTC fields to nanoseconds and back are small, and need no heap.
- Amend ADR 0016 to use `Clock`.
- A section in the Rose Book.
- The TSC, calibrated against the PIT, is #193. It can serve `Monotonic` at a finer resolution behind the same interface.
