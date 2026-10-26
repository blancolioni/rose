# 7. Networking

- **Status:** Proposed
- **Date:** 2026-10-26
- **Related:** #290 (this decision), [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0006](0006-signals.md) (signals), #291 to #301 (Networking project)

## Context

Rose has no networking. This ADR sets out where the network code runs, what a network capability grants, how names are resolved, and what happens to the network after a restore.

### What exists today

- **Drivers run in user space.** A driver gets port I/O, interrupt and device memory caps from init, and serves an IDL interface, as `ata` and `keyboard` do.
- **IPC is synchronous.** A send with `Block` waits until the receiver is receiving on that endpoint. A send without `Block` fails with `Request_Would_Block` if it isn't.
- **The timer can lose expiries.** `Timer.Set_Timer` takes a cap, and the timer sends to it when the time is up, without `Block` (`Timer.Server`). If the process that set the timer isn't receiving at that moment, the expiry is lost.
- **A server defers a request per endpoint.** `Rose.Server.Block_Current_Request` holds the current request and stops receiving on that endpoint until `Unblock_Endpoint`. `event-input-stream` blocks a `Read` this way. Every instance of an interface shares its endpoints, so this defers at most one request for all of them.
- **Restore notification.** A process that implements `Restorable` is sent `Restored` at its first receive after a restore (ADR 0004).
- **DMA.** A driver can be given identity-mapped pages from the low memory that init reserves (#291).

### Constraints

- **No ambient authority (ADR 0002).** A command that uses the network must say so in its signature, and receive only what the signature names.
- **No global namespace.** There is no system-wide host table or resolver.
- **Single-threaded servers.** A server that blocks on one request stalls all the others.
- **Persistence.** Processes and caps survive a restore. The far end of a connection does not: after a restore it has long since timed out.

### Reference

Ironclad (https://ironclad-os.org) has a TCP/IP stack in Ada, licensed GPL v3 or later like Rose: Ethernet, ARP, IPv4, UDP, TCP, DHCP and DNS. It runs inside its kernel, and its TCP has no retransmission or congestion control.

## Decision

### 1. Three kinds of process

- **A driver for each network card.** It owns the card and serves `Network_Device` (#294): send a frame, take a received frame, the MAC address, the MTU and the link state. It knows nothing about protocols.
- **`net`, one process for the whole stack.** Ethernet, ARP, IPv4, ICMP, UDP, TCP and the DHCP client all run in `net`. It is the only client of the drivers, and it serves the network interfaces in section 3.
- **`dns`, the resolver, a process of its own.** It is a client of `net` like any other, through a datagram cap (section 4).

Protocol code is ported from Ironclad where it exists. Ironclad's glue to its kernel is replaced by `Network_Device`, the timer and `Rose.Server`. Copied code keeps its copyright notices. The parsers for frames, headers and DNS responses handle untrusted input and are candidates for SPARK (#42).

### 2. Calls go down, signals come up

Two processes that can each call the other can deadlock: each blocks in a call while the other is blocked calling it. So the network follows one rule:

- **Calls go towards the hardware.** Clients call `net`, and `net` calls the drivers. A driver never calls `net`, and `net` never calls a client.
- **Answers come back as replies.** `net` may hold a client's request and reply later (section 5).
- **Anything else comes back as a signal** ([ADR 0006](0006-signals.md)). Sending a signal never blocks, and a signal sent while the receiver is busy is kept until it next receives, so it can't be lost.

- **Received frames.** When a frame arrives, the driver signals `net`. `net` then calls `Receive_Frame` until the driver has nothing left. A frame that arrives while `net` is busy is picked up when it next receives.
- **Timer expiries are signals.** TCP's retransmission timer depends on expiries never being lost, which today's timer doesn't guarantee (ADR 0006).
- **Frames cross one at a time,** in the invocation buffer, as #294 suggests. Rings in shared pages, which save a copy, can replace this later without changing the rule.

### 3. Network capabilities

What a network cap grants is part of its type, as with directories (ADR 0003): each interface is a narrower grant than the one above it.

```ada
interface Network is
   function Get_Host (Address : String) return interface Network_Host;
   function Restrict
     (Address       : String;
      Prefix_Length : Natural)
      return interface Network;
   function Get_Local_Port (Port : Natural) return interface Local_Port;
end Network;

interface Network_Host is
   function Address return String;
   function Get_Service (Port : Natural) return interface Network_Service;
   function Echo (Timeout : Natural) return Natural;
end Network_Host;

interface Network_Service is
   function Connect return interface Connection;
   function Open_Datagram return interface Datagram;
end Network_Service;

interface Local_Port is
   function Listen return interface Listener;
end Local_Port;

interface Listener is
   function Accept return interface Connection;
end Listener;

interface Connection : Stream_Reader, Stream_Writer is
   procedure Close;
end Connection;

interface Datagram is
   procedure Send (Buffer : Storage_Array);
   procedure Receive
     (Buffer : out Storage_Array;
      Last   : out Storage_Count);
end Datagram;
```

- **`Network`** grants a set of remote addresses: all of them for the cap `net` gives init, or one network after `Restrict`. `Get_Host` fails for an address outside the set. It also grants local ports through `Get_Local_Port`; a restricted network grants none.
- **`Network_Host`** grants one remote address, on any port. `Echo` is ping: it sends an ICMP echo request and returns the round-trip time in milliseconds, or fails after `Timeout`.
- **`Network_Service`** grants one address and one port. It is the narrowest grant, for a command that talks to one server. It is kept because grants should be as tight as possible, but no command needs it yet, so its use is unproven.
- **`Local_Port`** grants listening on one local port. Ports for outgoing connections are allocated by `net` and are not a grant.
- **A connection is a stream.** `Connection` is `Stream_Reader` plus `Stream_Writer`, so a command that takes a stream can be given a connection. `Close` closes it in both directions.
- **A datagram endpoint is connected.** `Datagram` sends to, and receives from, the one address and port it was opened for. Each endpoint has a bounded queue of received datagrams.
- **Addresses are strings,** such as `"10.0.2.2"`. `net` parses them. This keeps addresses out of the interfaces' types, so IPv6 can come later without changing them.
- **Commands take the narrowest cap they need.** `ping` takes `Host : in Network_Host`. A command that fetches one URL takes a `Network_Service`, and one that follows links takes a `Network` and a `Resolver`.

### 4. Names come from a resolver cap

```ada
interface Resolver is
   function Resolve (Name : String) return interface Network_Host;
end Resolver;
```

- **`dns` serves `Resolver`.** It is launched with a `Network` cap, the set of hosts it may return, and a `Network_Status` cap that reports the name servers that DHCP gave `net`. `Network_Status` is read-only: it gives the local address, the gateway and the name servers, as strings.
- **A resolver can't widen authority.** `dns` makes the caps it returns by calling `Get_Host` on its own `Network` cap, so it can only return hosts within that network. A resolver launched with a restricted network resolves only to hosts in it.
- **It queries over a datagram cap** for port 53 on the name server, and caches answers for their time to live.

### 5. `net` defers requests per connection

A `Read` on a connection with no data, an `Accept` with no connection waiting, or an `Echo` waiting for its reply must not stop `net` from serving anything else.

- **`net` holds each such request** and replies when it can be answered, or when it times out. A client blocked in `Read` on one connection blocks only itself.
- **`Rose.Server` needs to defer a request for one instance,** not a whole endpoint. A deferred request is held with its reply cap and the instance it was for, and the server replies to it later.

### 6. Persistence

After a restore, caps keep their authority, but the state of the network does not survive.

- **The driver implements `Restorable`.** When restored, it maps its DMA pages again, resets the card and sets up its queues (#291).
- **`net` implements `Restorable`.** When restored, it:
  - closes every TCP connection as if the peer had reset it. Requests it was holding fail with `Closed`, and so does every later call on those connections except `Close`;
  - keeps listeners and datagram endpoints, and empties their queues;
  - empties the ARP cache;
  - runs DHCP again, asking for the address it had (INIT-REBOOT, RFC 2131).
- **`Network`, `Network_Host`, `Network_Service` and `Local_Port` caps are unaffected.** They hold authority, not state. A command that held a host cap before a restore can still connect to it.
- **`dns` implements `Restorable`,** and empties its cache, since it has no way to tell how long the system was down.

### 7. Who holds what

- **`net` gives init a `Network` cap for every address,** and a `Network_Status` cap.
- **Init gives `dns` a `Network` and a `Network_Status`,** and gives petal a `Network` and the `Resolver`. These are roles in petal's declaration (ADR 0002, section 1), like `User` and `Home` (ADR 0003, section 5).
- **Petal binds network arguments from words.** A word for an `in Network_Host` argument is an address, looked up with `Get_Host`, or a name, looked up with `Resolve`. A `Network_Service` argument is written `host:port`. The command receives only the cap that results.
- **Changing the network's configuration** (a static address, the DHCP lease) is a separate cap that stays with init.

## Consequences

**Benefits**
- A command's signature says exactly which hosts or services it can reach.
- A crash in a driver or the resolver doesn't take the stack with it.
- The resolver is an ordinary cap, so a sandboxed command can be given one that resolves only within a restricted network, or none.
- Listeners and datagram endpoints survive a restore, so a server keeps listening without being set up again.
- Signals fix the timer for every user, not just the network.

**Costs**
- **It depends on signals** (ADR 0006), a kernel change.
- **A `Rose.Server` change** to defer requests per instance.
- **IPC for every frame,** twice on receive (the signal and `Receive_Frame`) and once on send. Shared rings can reduce this later.
- **`net` is a single point of failure** for every connection, and it serves them all from one thread.
- **New TCP work.** Retransmission and congestion control are not in Ironclad and must be written.

## Alternatives considered

- **The stack in the kernel, as in Ironclad.** This was never a serious option, and is recorded so the reasons are written down. Rose keeps drivers and servers out of the kernel. A stack there would put protocol parsing, which handles untrusted input from the network, in the most privileged code, and a fault in it would take down the whole system.
- **One process per connection,** as ADR 0003 does for files. Every connection shares the IP layer, the ARP cache and the timers, so each segment would cost IPC between processes, and the kernel's process limit would cap the number of connections.
- **The driver inside `net`.** This saves IPC for every frame, but a fault in the driver would take down the stack, and two cards would need two drivers in one process.
- **Frames by blocking calls in both directions.** This deadlocks when `net` sends a frame while the driver is delivering one.
- **Polling the driver on a timer, instead of signals.** This needs no kernel change, but adds latency to every received frame, and still depends on the timer's expiries arriving.
- **Sockets with ambient access.** A process could reach any host, which ADR 0002 rules out.

## Open questions

- **Revoking a connection.** When a job's membrane is revoked (ADR 0001), its connection caps stop working, but `net` isn't told, so the connections stay open. Should `net` be told when a cap to one of its instances is rescinded, or time out idle connections?
- **UDP servers.** A datagram endpoint is connected to one host. A server on a local port needs to receive from any host and reply to the sender. Does `Local_Port` give a datagram endpoint that can reply only to the hosts it has heard from?
- **More than one card.** `net` with two drivers needs routes and a choice of card for each destination. This waits until there are two cards.
- **IPv6 and fragmentation.** Neither is in scope. Addresses as strings keep the interfaces ready for IPv6.
- **Time after a restore.** A DHCP lease and DNS answers have lifetimes, so `net` and `dns` would need to know how long the system was down to keep them. Until there is a clock that survives a reboot, both start again.

## Follow-up work

- Signals, and the timer's use of them: see ADR 0006.
- `Rose.Server`: defer a request for one instance (new issue).
- IDL: `Network`, `Network_Host`, `Network_Service`, `Local_Port`, `Listener`, `Connection`, `Datagram`, `Network_Status` and `Resolver` (new issue).
- Petal: bind network arguments, and take `Network` and `Resolver` roles (new issue).
- The existing issues, #291 to #301, with these changes:
  - #294: receiving is by signal and `Receive_Frame`; the driver never calls `net`.
  - #297: `ping` takes a `Network_Host` and uses `Echo`.
  - #298: datagram endpoints are connected, opened from a `Network_Service`.
  - #300: `dns` serves `Resolver`, and gets its name servers from `Network_Status`.
  - #301: connections are `Connection` caps, closed on restore.
