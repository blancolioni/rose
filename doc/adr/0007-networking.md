# 7. Networking

- **Status:** Accepted
- **Date:** 2026-10-26
- **Revised:** 2026-11-27, to follow the ADRs written since (interface groups, signals, abilities as interfaces, privilege, petal paths, the clock, random numbers, copy before write, replacement)
- **Related:** #290 (this decision), #1182 (0.5.0 networking), [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0006](0006-signals.md) (signals), [ADR 0016](0016-random-numbers.md) (random numbers), [ADR 0020](0020-clock.md) (clock), [ADR 0021](0021-privilege.md) (privilege), [ADR 0030](0030-copy-before-write-during-a-checkpoint.md) (copy before write), [ADR 0032](0032-replacing-a-running-process.md) (replacement), [ADR 0034](0034-interface-packages.md) (interface packages), [ADR 0044](0044-petal-paths-as-uri-references.md) (petal paths), #1148 (interfaces for abilities), #291 to #301 (project 59 (Networking))

## Context

Rose has no networking. This ADR sets out where the network code runs, what a network capability grants, how names are resolved, and what happens to the network after a restore.

### What exists today

- **Persistent processes own devices.** A process gets port I/O, interrupt and device memory caps from init, and serves an IDL interface, as `ata` and `keyboard` do.
- **IPC is synchronous.** A send with `Block` waits until the receiver is receiving on that endpoint. A send without `Block` is refused if it isn't.
- **Signals exist** (ADR 0006, PR #709). `Caps.Create_Signal` makes a signal cap, and a process that serves `Processes.Signalled` is sent the bits that were raised. Sending a signal never blocks, and bits raised while the receiver is busy are kept until it next receives.
- **The timer can still lose expiries.** `Time.Timer` sends to a cap without `Block`, and if the process that set the timer isn't receiving at that moment, the expiry is lost. #1073 makes the timer report expiries through signal caps only.
- **A server defers a request per endpoint.** `Rose.Server.Block_Current_Request` holds the current request and stops receiving on that endpoint until `Unblock_Endpoint`. Every instance of an interface shares its endpoints, so this defers at most one request for all of them. #1072 defers requests per instance.
- **One open call per process.** A process that is waiting in a call can't receive, until open calls with call numbers (#1068). Ada tasks are user-level threads (ADR 0043), planned for 0.6.0, so a 0.5.0 server is single-threaded and event-driven.
- **Restore notification.** A process that implements `Processes.Restorable` is sent `Restored` at its first receive after a restore (ADR 0004).
- **DMA.** A process can be given identity-mapped pages from the low memory that init reserves (#291). They are not persistent pages, and ADR 0030 (section 8) makes that a rule for every DMA buffer.
- **Real time survives a restore.** The clock's real time includes the time the system was down (ADR 0020).

### Constraints

- **No ambient authority (ADR 0002).** A command that uses the network must say so in its signature, and receive only what the signature names.
- **No global namespace.** There is no system-wide host table or resolver.
- **Single-threaded servers.** A server that blocks on one request stalls all the others.
- **Persistence.** Processes and caps survive a restore. The far end of a connection does not: after a restore it has long since timed out.
- **Narrow grants are interfaces (#1148).** A narrower grant is an interface that the wider one inherits, not a wider interface whose operations fail.
- **Endpoints.** A process has at most 64 endpoints, one for each operation of each interface it serves.

### Reference

Ironclad (https://ironclad-os.org) has a TCP/IP stack in Ada, licensed GPL v3 or later like Rose: Ethernet, ARP, IPv4, UDP, TCP, DHCP and DNS. It runs inside its kernel, and its TCP has no retransmission or congestion control.

## Decision

### 1. Three kinds of process

- **A process for each network card,** such as `virtio_net` (#295). It owns the card and serves `Devices.Network_Device` (#294): send a frame, take a received frame, the MAC address, the MTU and the link state. It knows nothing about protocols.
- **`net`, one process for the whole stack.** Ethernet, ARP, IPv4, ICMP, UDP, TCP and the DHCP client all run in `net`. It is the only client of the card processes, and it serves the network interfaces in section 3.
- **`dns`, the resolver, a process of its own.** It is a client of `net` like any other, through a datagram cap (section 4).

All three are persistent processes, not boot modules: nothing on the pager path (ADR 0008, ADR 0039) needs the network.

Protocol code is ported from Ironclad where it exists. Ironclad's glue to its kernel is replaced by `Devices.Network_Device`, the timer and `Rose.Server`. Ironclad's code may assume kernel threads or blocking waits. In `net` it has to run from events instead (section 2). Copied code keeps its copyright notices. The parsers for frames, headers and DNS responses handle untrusted input and are candidates for SPARK (#42).

### 2. Calls go down, signals come up

Two processes that can each call the other can deadlock: each blocks in a call while the other is blocked calling it. So the network follows one rule:

- **Calls go towards the hardware.** Clients call `net`, and `net` calls the card processes. A card process never calls `net`, and `net` never calls a client.
- **Answers come back as replies.** `net` may hold a client's request and reply later (section 5).
- **Anything else comes back as a signal** (ADR 0006).

The rule applies like this:

- **Received frames.** When a frame arrives, the card process raises a signal to `net`. `net` then calls `Receive_Frame` until the card process has nothing left. A frame that arrives while `net` is busy is picked up when it next receives.
- **Timer expiries are signals** (#1073). TCP's retransmission timer depends on expiries never being lost.
- **`net` never makes a slow call.** Its only calls are to the card processes, which answer at once. With one open call per process, a slow call would stop `net` from receiving.
- **Frames cross one at a time,** in the invocation buffer, as #294 suggests. Rings in shared pages, which save a copy, can replace this later without changing the rule.

### 3. Network capabilities

What a network cap grants is part of its type, as with directories (ADR 0003). Each ability is an interface, and a wider grant inherits the narrower ones (#1148). The interfaces are in a new group, `Network` (ADR 0034).

```ada
interface Network.Remote_Hosts is
   function Get_Host (Address : String) return interface Network.Host;
   function Restrict
     (Address       : String;
      Prefix_Length : Natural)
      return interface Network.Remote_Hosts;
end Network.Remote_Hosts;

interface Network.Local_Ports is
   function Get_Local_Port (Port : Natural) return interface Network.Local_Port;
end Network.Local_Ports;

interface Network.Access : Network.Remote_Hosts, Network.Local_Ports is
end Network.Access;

interface Network.Host is
   function Address return String;
   function Get_Service (Port : Natural) return interface Network.Service;
   function Echo (Timeout : Natural) return Natural;
end Network.Host;

interface Network.Service is
   function Connect return interface Network.Connection;
   function Open_Datagram return interface Network.Datagram;
end Network.Service;

interface Network.Local_Port is
   function Listen return interface Network.Listener;
end Network.Local_Port;

interface Network.Listener is
   function Accept return interface Network.Connection;
end Network.Listener;

interface Network.Connection : Streams.Reader, Streams.Writer is
   procedure Close;
end Network.Connection;

interface Network.Datagram is
   procedure Send (Buffer : Stream_Element_Array);
   procedure Receive
     (Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
end Network.Datagram;
```

- **`Network.Remote_Hosts`** grants a set of remote addresses: all of them, or one network after `Restrict`. `Get_Host` raises for an address outside the set. `Restrict` returns a `Remote_Hosts`, so a restricted cap has no way to ask for local ports, rather than a way that always fails.
- **`Network.Local_Ports`** grants listening on local ports.
- **`Network.Access`** is both. It is what `net` gives init, and nothing else holds it unless init passes it on.
- **`Network.Host`** grants one remote address, on any port. `Echo` is ping: it sends an ICMP echo request and returns the round-trip time in milliseconds, or raises after `Timeout`.
- **`Network.Service`** grants one address and one port. It is the narrowest grant, for a command that talks to one server. It is kept because grants should be as tight as possible, but no command needs it yet, so its use is unproven.
- **`Network.Local_Port`** grants listening on one local port. Ports for outgoing connections are allocated by `net` and are not a grant.
- **A connection is a byte stream.** `Network.Connection` is `Streams.Reader` plus `Streams.Writer`, so a command that takes a stream can be given a connection. `Close` closes it in both directions.
- **A datagram endpoint is connected.** `Network.Datagram` sends to, and receives from, the one address and port it was opened for. Each endpoint has a bounded queue of received datagrams.
- **Operations fail by raising** (idle#60). A call on a connection that has been closed, by `Close`, by the peer, or by a restore (section 6), raises `Closed`. A call that runs out of time raises `Timed_Out`.
- **Addresses are strings,** such as `"10.0.2.2"`. `net` parses them. This keeps addresses out of the interfaces' types, so IPv6 can come later without changing them.
- **Commands take the narrowest cap they need.** `ping` takes `Host : in Network.Host`. A command that fetches one URL takes a `Network.Service`, and one that follows links takes a `Network.Remote_Hosts` and a `Network.Resolver`.
- **`net` serves about 30 operations,** with `Processes.Restorable` and `Processes.Replaceable`, which is within the limit of 64 endpoints.

### 4. Names come from a resolver cap

```ada
interface Network.Resolver is
   function Resolve (Name : String) return interface Network.Host;
end Network.Resolver;

interface Network.Status is
   function Local_Address return String;
   function Gateway return String;
   function Name_Servers return String;
end Network.Status;
```

- **`dns` serves `Network.Resolver`.** It is launched with a `Network.Remote_Hosts` cap, the set of hosts it may return, and a `Network.Status` cap. `Network.Status` is read-only: it gives the local address, the gateway and the name servers that DHCP gave `net`, as strings.
- **A resolver can't widen authority.** `dns` makes the caps it returns by calling `Get_Host` on its own `Remote_Hosts` cap, so it can only return hosts within that set. A resolver launched with a restricted cap resolves only to hosts in it.
- **It queries over a datagram cap** for port 53 on the name server, and caches answers for their time to live, measured in real time (ADR 0020).

### 5. `net` defers requests per connection

A `Read` on a connection with no data, an `Accept` with no connection waiting, or an `Echo` waiting for its reply must not stop `net` from serving anything else.

- **`net` holds each such request** and replies when it can be answered, or when it times out. A client blocked in `Read` on one connection blocks only itself.
- **`Rose.Server` defers a request for one instance,** not a whole endpoint (#1072). A deferred request is held with its reply cap and the instance it was for, and the server replies to it later. More than one request can be held for each handler.

### 6. Persistence

After a restore, caps keep their authority, but the state of the network does not survive.

- **The card process implements `Processes.Restorable`.** When restored, it maps its DMA pages again, resets the card and sets up its queues (#291).
- **`net` implements `Processes.Restorable`.** When restored, it:
  - closes every TCP connection as if the peer had reset it. Requests it was holding raise `Closed`, and so does every later call on those connections except `Close`;
  - keeps listeners and datagram endpoints, and empties their queues;
  - empties the ARP cache, since the hosts on the link may have changed;
  - runs DHCP again, asking for the address it had (INIT-REBOOT, RFC 2131). The machine may have moved to another network while it was down, so the old lease is confirmed rather than assumed, even when it hasn't expired.
- **`Remote_Hosts`, `Local_Ports`, `Host`, `Service` and `Local_Port` caps are unaffected.** They hold authority, not state. A command that held a host cap before a restore can still connect to it.
- **`dns` implements `Processes.Restorable`,** and keeps the answers in its cache whose time to live hasn't run out in real time, which includes the time the system was down (ADR 0020).

### 7. Persistent pages and checkpoints

- **DMA buffers are #291's low-memory pages.** They are identity-mapped, never `mem`'s, and never persistent, as ADR 0030 (section 8) requires.
- **The rest of a card process's working set must stay mapped.** ADR 0030 (section 7) leaves it to the network work to say how a process that must keep up with hardware asks `mem` not to page out its code, stack and buffers. Until there is a way to ask, the card process touches its working set at start-up, as ADR 0030 says, and the way to ask is follow-up work.

### 8. Replacing the network processes

An update (ADR 0031) replaces persistent processes, so `net` and `dns` implement `Processes.Replaceable` (ADR 0032).

- **`net`'s state stream** carries its listeners and datagram endpoints, its DHCP lease and its address. TCP connections are closed, as at a restore. A handover keeping connections would need the TCP state in the stream, and is left for later.
- **`dns`'s state stream** carries its cache.
- **A card process** hands the card over as any process holding device caps does.

### 9. Randomness

TCP's initial sequence numbers, ephemeral ports and DNS query identifiers must be unpredictable, or an attacker off the path can forge segments and answers.

- **`net` and `dns` are each given a `Random` cap** (ADR 0016).
- **A card process is given an `Entropy_Source`,** and feeds it the arrival times of frames, as ADR 0016 (section 4) plans.

### 10. Who holds what

- **`net` gives init a `Network.Access` cap,** and a `Network.Status` cap.
- **Init gives `dns` a `Remote_Hosts` and a `Status`.**
- **Remote hosts and the resolver are public roles.** Petal is given a `Remote_Hosts` and the `Resolver`, and binds them like `User` and `Home` (ADR 0003, section 5). Reaching out to other hosts is what most network commands do, and a command still gets only the hosts its arguments name.
- **Listening is privileged.** A role for `Local_Ports` or `Local_Port` has the category `network` (ADR 0021). Petal doesn't hold it. A command that listens asks the broker, through `sudo network`.
- **Changing the network's configuration** (a static address, renewing or releasing the DHCP lease) is a `Network.Configuration` cap whose role has the category `network`, granted by the broker. `ifconfig` is the first command that would take it, as ADR 0021 expects.

### 11. Petal binds network arguments by their type

Petal knows each argument's type from the command's signature, and parses a network word for that type. It is never a path (ADR 0044).

- **An `in Network.Host` argument** is an address, looked up with `Get_Host`, or a name, looked up with `Resolve`.
- **An `in Network.Service` argument** is written `host:port`, where `host` is an address or a name.
- **The word isn't a URI.** `example.org:80` would read as a URI with the scheme `example.org`, so a path argument and a network argument can't share a syntax. A network word never starts with `//` or `/`, so it can't be taken for a path.
- **The command receives only the cap that results.**

ADR 0044 leaves schemes for later, and network names are its example. A scheme naming a resolver that petal holds, such as `dns:example.org:80`, would let one word name a host anywhere a word can appear. That waits until something needs it.

## Consequences

**Benefits**
- A command's signature says exactly which hosts or services it can reach.
- A crash in a card process or the resolver doesn't take the stack with it.
- The resolver is an ordinary cap, so a sandboxed command can be given one that resolves only within a restricted set of hosts, or none.
- A narrowed cap has no operations it can't use: `Restrict` returns a cap without local ports.
- Listening needs elevation, and reaching out doesn't.
- Listeners and datagram endpoints survive a restore and a replacement, so a server keeps listening without being set up again.
- The resolver's cache survives a restore for as long as its answers are valid.

**Costs**
- **`Rose.Server` deferral per instance** (#1072) and **timer expiries by signal** (#1073) come first.
- **IPC for every frame,** twice on receive (the signal and `Receive_Frame`) and once on send. Shared rings can reduce this later.
- **`net` is a single point of failure** for every connection, and it serves them all from one thread.
- **New TCP work.** Retransmission and congestion control are not in Ironclad and must be written.
- **A replacement closes every connection.**
- **Ported code may need reworking** where Ironclad assumes threads or blocking waits.

## Alternatives considered

- **The stack in the kernel, as in Ironclad.** This was never a serious option, and is recorded so the reasons are written down. Rose keeps device code and servers out of the kernel. A stack there would put protocol parsing, which handles untrusted input from the network, in the most privileged code, and a fault in it would take down the whole system.
- **One process per connection,** as ADR 0003 does for files. Every connection shares the IP layer, the ARP cache and the timers, so each segment would cost IPC between processes, and the kernel's process limit would cap the number of connections.
- **The card's code inside `net`.** This saves IPC for every frame, but a fault in it would take down the stack, and two cards would need two sets of device code in one process.
- **Frames by blocking calls in both directions.** This deadlocks when `net` sends a frame while the card process is delivering one.
- **Polling the card process on a timer, instead of signals.** This adds latency to every received frame, and still depends on the timer's expiries arriving.
- **One `Network` interface whose `Get_Local_Port` fails once restricted,** as this ADR first had it. A command couldn't tell from the type what it was given (#1148).
- **Listening as a public role.** Any command given a `Local_Port` could accept connections from other hosts, and the user wouldn't be asked.
- **Network words as URIs now,** such as `tcp://example.org:80`. It adds a syntax that nothing yet needs, and the argument's type already says what the word is.
- **Sockets with ambient access.** A process could reach any host, which ADR 0002 rules out.

## Open questions

- **Revoking a connection.** When a job's membrane is revoked (ADR 0001), its connection caps stop working, but `net` isn't told, so the connections stay open. Should `net` be told when a cap to one of its instances is rescinded, or time out idle connections? ADR 0018 (#508) expects to suspend network caps after a restore until the user authenticates again, which needs the same thing.
- **UDP servers.** A datagram endpoint is connected to one host. A server on a local port needs to receive from any host and reply to the sender. Does `Local_Port` give a datagram endpoint that can reply only to the hosts it has heard from?
- **More than one card.** `net` with two card processes needs routes and a choice of card for each destination. This waits until there are two cards.
- **IPv6 and fragmentation.** Neither is in scope. Addresses as strings keep the interfaces ready for IPv6.
- **Installed network caps.** ADR 0029 installs only lookup-only directory, file and launch caps. A program that always talks to one service, such as an update fetcher, might want an installed `Network.Service`. Not needed yet.

## Follow-up work

- `Rose.Server`: defer requests per instance (#1072).
- Timer: expiries through signal caps only (#1073).
- IDL: the `Network` group: `Remote_Hosts`, `Local_Ports`, `Access`, `Host`, `Service`, `Local_Port`, `Listener`, `Connection`, `Datagram`, `Status`, `Configuration` and `Resolver` (new issue).
- Petal: bind network arguments by type (section 11), and take the `Remote_Hosts` and `Resolver` roles (new issue).
- `mem`: a way for a process to keep its working set mapped (section 7; new issue).
- The existing issues, #291 to #301, with these changes:
  - #294: the interface is `Devices.Network_Device`; receiving is by signal and `Receive_Frame`; the card process never calls `net`; it feeds an `Entropy_Source`.
  - #296: `net` is `Restorable` and `Replaceable`, takes a `Random` cap, and gives init `Network.Access`.
  - #297: `ping` takes a `Network.Host` and uses `Echo`.
  - #298: datagram endpoints are connected, opened from a `Network.Service`.
  - #300: `dns` serves `Network.Resolver`, gets its name servers from `Network.Status`, takes a `Random` cap, and keeps unexpired answers across a restore.
  - #301: connections are `Network.Connection` caps, closed on restore and on replacement.
