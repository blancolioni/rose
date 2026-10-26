# 5. A persistent console

- **Status:** Accepted
- **Date:** 2026-10-26
- **Related:** #269 (this decision), [ADR 0004](0004-restore-notification.md) and #268 (restore notification), #102 (restore at boot)

## Context

The console is boot module 3. Init launches it at every boot, gives it a cap that maps the VGA text page and a cap for the cursor ports, and gives its `Stream_Writer` caps to everything else. Like every boot module, it is not persistent.

Since restore (#102), a reboot brings back the persistent processes as they were at the last checkpoint, but not the console:

- **The screen is lost.** What petal and the other persistent processes had written is gone, and the screen shows the messages of the boot that did the restore.
- **Persistent processes write to the boot console.** Exec, command, keyboard and petal all hold caps to boot module 3. Boot modules come back with the same object ids, so the caps still work after a restore, but they reach a new console that knows nothing of the old screen.
- **Exec gives its console cap to every program it launches** (`Exec.Console_Cap`, cap 5). Exec is installed first, while the boot console is the only console.

The VGA page is device memory. The memory server doesn't manage it, so it isn't in a checkpoint, and the kernel maps it straight into the process (`Identity_Map_Page`), so a restored process doesn't have the mapping.

## Decision

### 1. A persistent console driver, `vga-console`

`vga-console` is installed from the install image like the other drivers, straight after exec. Its `.driver` file asks for the VGA text page (a new `device-page` cap type in `configure-driver`), the CRT controller's index and data ports, and the data port for reading.

- **It keeps a copy of the screen in its own memory:** rows 1 to 24, as VGA cells, and the cursor position. Every write changes the copy and the VGA page. The copy is ordinary process memory, so the memory server saves it with the process's other pages, and a checkpoint holds the screen.
- **Row 0 stays the kernel's status line.**
- **It takes over the screen as it finds it.** At its first write it copies rows 1 to 24 from the VGA page and reads the cursor position from the CRT controller, so the install messages already on the screen stay there.

### 2. Handing over from the boot console

When restore has launched `vga-console`, it sends init the console's `Stream_Writer` interface, as it does for the keyboard's input stream. Init then:

1. tells the boot console to send everything it is given on to the new console, through a new interface, `Output_Redirect`;
2. tells exec to use the new console as the standard output it gives to the programs it launches, and for its own output, through a new `Exec` operation, `Set_Standard_Output`;
3. uses the new console itself, as the standard output of everything it installs or launches from then on: command, keyboard, event-input-stream, the tests and petal.

The boot console sends its writes on, rather than stopping, because boot modules and restore still hold caps to it.

The boot console registers the `Redirect` endpoint with `Rose.Server` itself, rather than through the generated `Output_Redirect` server package. Every generated server package has its own instance table (#276), and a boot module started before the memory server takes its pages from a fixed kernel heap. One more table in the boot console was enough to exhaust that heap at boot.

Init used to treat the second install message as command's. It now checks the name, because `vga-console` comes between exec and command.

### 3. Restoring the screen

`vga-console` implements `Restorable` (ADR 0004). When it is told that it has been restored, it maps the VGA page again, copies its saved screen to rows 1 to 24, and puts the cursor back. The screen is then exactly as it was at the checkpoint.

After a restore, the boot console comes back without a redirect. Just before restore resumes the restored processes, init tells it to discard what it is given, so that boot modules don't write over the restored screen.

### 4. Device pages stay writable during a checkpoint

While a checkpoint is active, the kernel makes every writable page of a persistent process read-only, and gives a blocked write to the memory server as a page fault when the checkpoint ends. The memory server doesn't manage device pages, so a console write during a checkpoint would fail. The kernel now marks a page mapped by `Identity_Map_Page` as a device page, in a spare bit of the page table entry, and the checkpoint leaves device pages writable. Their contents aren't part of a checkpoint anyway.

## Consequences

- **A restore puts back the screen as it was at the checkpoint,** including what petal had written and where the cursor was.
- **Output written after the last checkpoint is lost from the screen,** like everything else written since then.
- **After a restore, boot modules' messages don't appear on the screen.** The kernel's own messages still go to the serial port.
- **Restore still relies on the boot order,** because the boot console's caps in non-persistent processes are found by object id. Persistent processes no longer hold boot console caps, except where a process saved one before the handover (none do today).
- **Any `.driver` file can ask for a device page,** as it can already ask for I/O ports. The install image decides what each driver gets.

## Alternatives considered

- **Start the console before exec.** Exec is how everything else is installed and launched, so the console would have to be launched as a boot module, which isn't persistent.
- **Only redirect the boot console.** Exec's console cap would still reach a boot console after a restore, so the keyboard's echo and everything exec launches would write to it, not to the restored console.
- **Save the VGA page itself in the checkpoint.** The memory server and the checkpoint format would have to handle device pages, and a restore would still need to know when to put the page back.
