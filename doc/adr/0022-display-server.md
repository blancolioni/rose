# 22. A display server on the framebuffer

- **Status:** Proposed
- **Date:** 2026-10-31
- **Related:** #578 (this decision), project 54 (the implementation), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0005](0005-persistent-console.md) (the persistent console), [ADR 0013](0013-typed-streams.md) (typed streams), [ADR 0014](0014-banks.md) (banks)

## Context

Rose's only display is the VGA text page. `vga-console` owns it, keeps a copy of the screen, reads the keyboard and is the line discipline (ADR 0005). That is enough for a shell, and it can't show anything else.

A graphics display needs a linear framebuffer, which raises the question of who may draw on it. The usual answers don't fit:

- **X assumes ambient authority.** Any client can read another client's window, capture the keyboard and inject input. Running it would mean one sandbox around the whole thing, or giving up what the capability model is for.
- **Wayland and Nano-X need a POSIX layer** (sockets, `mmap`, `select`) that Rose doesn't have, and Wayland assumes a GPU and `dmabuf`.
- **Rose is getting a pointer,** so the design has to route pointer input as safely as keys. X sends it to whoever asks.

What Rose does have is the means to build the right thing directly: caps for device memory (`device-page`, ADR 0005), typed streams (ADR 0013), shared memory through the memory server, and restore (ADR 0004). A pointer is one more input device, like the keyboard.

## Decision

### 1. The screen is a surface, with one holder

The boot code asks GRUB for a 32-bit graphics mode on every boot, and passes the framebuffer's address, pitch and size on to init (#582). Because the boot always gives the same kind of screen, a checkpoint always restores onto one.

**A `screen` driver owns the hardware.** It is installed like the other drivers, and its `.driver` file asks for the framebuffer, which init fills in from the boot. It also takes the keyboard's input and the pointer's stream (sections 3 and 4). No other process holds a cap to the framebuffer, the keyboard or the pointer. The driver gives out one *surface* for the whole screen, with the same interface as the surfaces that the display server gives its clients (section 2):

- **its buffer** is ordinary memory, shared with the surface's holder;
- **`Damage`** copies a region of the buffer to the framebuffer, converting the format if it differs;
- **its event stream** carries the keys and the pointer's events.

**The screen surface has one holder at a time: the console or the display server.**

- **Console mode:** the console holds the screen surface. It is the successor to `vga-console`, and it draws its text into the surface with a bitmap font, and keeps the line discipline and its copy of the screen (ADR 0005). It takes its keys from the surface's event stream and ignores the pointer's events. No display server runs (#653).
- **Graphical mode:** the display server holds the screen surface, and the console holds a surface that the display server gave it. Petal and everything else that writes to the console keep their caps to it, and only the surface under the console changes.

**Switching hands the cap over** (#655). To change to graphical mode, the console starts the display server with the screen surface, and asks it for a surface of its own. To change back, the display server gives the screen surface back to the console and stops. Nothing else is restarted, and the choice isn't stored anywhere: a checkpoint saves whoever holds the screen surface, and a restore brings them back. Console mode is what is installed, and it stays the default until the display server is in everyday use.

**If GRUB offers no framebuffer, Rose boots to the standard console.** There is no `screen` driver and no display server, and the console runs on the VGA text page, as `vga-console` does today. A system that had a framebuffer at an earlier boot and has none now is not handled here (see the open questions).

**Before the console has the screen,** nothing draws on it. The kernel's status line and the boot console write to the VGA text page, which isn't on the screen when there is a framebuffer. So with a framebuffer the status line goes, and the early messages go to the serial port only (#654).

**The server draws on a root surface.** It is written against one surface, which it draws into and takes its input from, and that surface has the same interface as the ones it gives to its clients (section 2). On the hardware, the root is the screen surface. A display server can also be given a surface from another display server, and then it draws into that. Either way, it takes keys and pointer events from the root surface's event stream, so it works the same way on the screen and nested. Running on another display server's surface is nesting. The nested server has a *virtual* pointer: its position is the one in the pointer events that arrive on the root surface, in that surface's coordinates, so it always sits where the parent's pointer is, relative to the surface. The nested server doesn't draw a cursor, because the parent draws the only one. A cursor-shape request from a nested client is passed up to the parent through the root surface, like any other client's. Nesting gives:

- **Headless tests** (#587): a surface backed by plain memory, with scripted events, stands in for the screen.
- **Sandboxing:** the windows of an untrusted client can live in a surface of their own. The client sees a screen, and the parent sees one surface.

### 2. Clients draw into surfaces

A client asks the display server for a *surface* and gets a cap to it. A surface is:

- **A pixel buffer**, always 32-bit XRGB, in memory that the client and the server both map. The client draws into it with whatever it likes (the drawing library, #579). The `screen` driver converts if the framebuffer's format differs (section 1).
- **`Damage (X, Y, Width, Height)`**, a call by which the client says that it has changed a region. The server copies that region to the screen. A surface that never calls it is never redrawn after the first time.
- **An event stream** (ADR 0013), from the server to the client. It carries key events, pointer events (section 4), a change of focus, and a change of size. After a size change the client asks the surface for its buffer again.

A client holds the caps to its own surface, so it can draw only there and can't see another client's pixels. How the buffer is shared depends on banks (ADR 0014). Until they exist, the server creates the buffer with the memory server and gives the client a cap that maps it.

**A window has an environment, and the display server doesn't look inside it.** The environment is a set of caps: the surface, which the display server provides, and whatever else the creator of the window adds (a directory, say). The creator gives the environment to whatever manages the window. That might be a terminal client running petal, a drawing program, or anything else that can use a surface. The display server doesn't know which, and nothing about a surface mentions petal or commands.

### 3. The server owns the keyboard

The display server takes the keys from its root surface's event stream, and sends each key event to the *focused* surface's event stream and to no other. A client that isn't focused receives nothing and has no way to ask for another surface's keys, or for the keyboard itself.

Line editing stays in the client. The display server forwards keys, and doesn't edit lines. A terminal in a window is the console on a surface that the display server gave it (#586).

### 4. The server owns the pointer

A pointer driver (a PS/2 mouse to begin with, #588) gives the `screen` driver a stream of movements and button changes, and they arrive as pointer events on the screen surface. The display server keeps the pointer's position and draws the cursor itself, on top of the composed screen. No client has a cap to the pointer driver. In console mode there is no cursor.

- **A pointer event goes to the surface under the cursor,** in that surface's own coordinates, and to no other. A client never learns where the pointer is outside its surface.
- **A button press on a surface focuses it.** Keys then go to it (section 3).
- **A button held down grabs:** until it is released, the surface where the press began receives the movement, even outside its area, in coordinates clipped to nothing the client could use to find other surfaces. The grab ends on release.
- **The cursor shape is the client's request.** A surface can ask for one of a few shapes from the server, and gets no access to the image.

The pointer is another event in the surface's event stream, so the surface interface doesn't change.

### 5. Layout is tiled, with the keyboard and the pointer

The server, not the clients, decides where each surface goes. It tiles the screen: one full-screen surface to begin with, then splits. A reserved key chord is handled by the server and never forwarded. It moves focus, splits, closes and swaps tiles (#585). The pointer does the same: a click focuses, and dragging a divider resizes the tiles on either side. A client is told its size in an event and doesn't choose it.

There is no stacking and no overlap to begin with, so the server needs no clipping against other windows, and a client can't cover another. Floating windows are the obvious next step now that there is a pointer, and they belong in a later ADR.

### 6. Persistence

- **Surfaces are ordinary memory,** so the memory server saves them with the client's other pages, and a checkpoint holds every surface as it was.
- **The framebuffer is a device page.** Its contents are not in a checkpoint, and a checkpoint leaves it writable (ADR 0005, section 4).
- **The `screen` driver is `Restorable`** (ADR 0004). When it is told that it has been restored, it maps the framebuffer again and copies its whole buffer to it. The screen is then as it was at the checkpoint, in either mode, and the holder of the screen surface does nothing.
- **Who holds the screen surface is in the checkpoint,** because the holders are persistent processes holding a persistent cap. A restore comes back in the mode it was saved in.
- **The surface caps are persistent,** so a client keeps its surface across a restore, with no work of its own.

### 7. Interfaces

The interfaces are declared in `interfaces/` (#581): the display server (create a surface, with the size it asks for), the surface (get its buffer, report damage, get its event stream, read the pointer's position, ask for a cursor shape), and the event type (key, pointer, focus, size). Operations that can fail are procedures that raise, not functions that return `Boolean` (idle#60).

## Consequences

- **No client can read or draw on another's surface,** and none can see a key that wasn't sent to it. This is the property that X can't give.
- **Nothing outside the `screen` driver holds the keyboard.** A program that wants keys has to be given a surface.
- **Console mode costs a copy too:** the console draws into the screen surface's buffer, and the `screen` driver copies the damage to the framebuffer. It needs the same shared mapping of a region as the display server's clients (#593).
- **A client costs one buffer** of width × height × 4 bytes. Two full-screen 1024×768 surfaces are about 6 MB.
- **Every draw is a copy.** The client draws into its buffer, and the server copies the damaged region. This is slower than drawing straight to the screen, and simple, and a screen of text is cheap.
- **A client sees only pointer events inside its own surface,** or during its own grab. It can't track the pointer across the screen, which X lets any client do.
- **The keyboard still works alone.** Every pointer action in the layout has a key chord, so the display is usable with no pointer.
- **The console moves to the screen surface** when there is a framebuffer, so text and graphics share a screen and either can be chosen without a reboot. This amends ADR 0005, which has the console on the VGA text page.
- **VGA text mode is kept** for systems that get no framebuffer from GRUB.
- **With a framebuffer, early messages are on the serial port only,** and the kernel's status line is gone.

## Alternatives considered

- **Choose text or graphics at boot,** with a GRUB entry for each. GRUB sets the video mode, and Rose can't change it afterwards, so the choice would be fixed for the boot. A checkpoint taken in one mode could then be restored into a boot in the other, with the clients' surfaces left with no display server.
- **The console and the display server each map the framebuffer,** and init stops one and starts the other to switch. Each would then have to be `Restorable` and redraw the screen, and each would need its own way to the keyboard and the pointer. Switching would need init, and a setting stored for it. Petal's console caps would have to be moved to a new console when the display server started.
- **Port X (Xfbdev) or Nano-X.** Both need sockets and `fork`, and X gives every client the screen and the keyboard.
- **Port Wayland.** It is closer to this design, since clients render into buffers, and it still wants sockets, `mmap` and `dmabuf`.
- **A Plan 9 style draw device** (`/dev/draw`, with rio on top). Clients send drawing commands, and the server holds the images and does the drawing and the fonts. The structure is close to this one: a server multiplexes the screen, the keyboard and the pointer, and a window sees only what it is given. It differs in where the pixels live. A draw-command server keeps every image in the server, so the checkpoint would have to save them, and it needs masks, compositing and fonts in the server. Pixel buffers in client memory come back with the client at restore, and the client can use any renderer. What this ADR takes from Plan 9 is that a window gets only its own slice of the devices, and that a window system can run inside another one (nesting, section 1). A draw-command layer for remote clients could be added later on top of the shared buffer.
- **Let clients draw straight to the framebuffer,** with the server arbitrating regions. A client would then hold a cap to the whole framebuffer, so one client could draw over the others.
- **Overlapping windows now.** They need clipping, damage tracking between windows and a stacking order. A tiled layout needs none, and floating windows can be added on the same surface interface.
- **A pointer cap for clients.** It would let every client read the pointer's position everywhere, as X does.

## Open questions

- **A framebuffer that was there and now isn't.** A checkpoint saved with a `screen` driver running, restored on a boot where GRUB gave no framebuffer. Future work.
- **How the mode is changed.** The command that asks the console to switch, and where the console gets the cap that starts the display server (#655).
- **The pixel format** if the mode GRUB gives isn't 32-bit. The `screen` driver could convert, or treat the mode as no framebuffer.
- **Who starts what runs in a window.** The server creates a window on request, for example from the key chord for a new window, and the request has to name or reach whatever receives the surface and manages it. The server might hold a cap to a launcher for this, or the request might carry one. Which, and where that cap comes from, is not decided here.
- **The cursor.** A software cursor needs the pixels under it saved and restored, or redrawn with each composed frame. Which is simpler depends on how the server composes.
- **Other pointers.** USB mice and touch need a USB stack, which Rose doesn't have. The pointer interface should not assume PS/2.
- **Vsync and tearing.** Copying while the display scans out can tear, and nothing in Rose gives a vertical blank.
- **How a surface's buffer is resized** while the client may still be drawing into the old one.
