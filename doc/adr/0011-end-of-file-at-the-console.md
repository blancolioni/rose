# 11. End of file at the console

- **Status:** Proposed
- **Date:** 2026-10-28
- **Related:** [ADR 0009](0009-files-open-readers.md) (files open readers), [ADR 0010](0010-resources.md) (resources), [ADR 0006](0006-signals.md) (signals), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands), #395 (standard input for an omitted input argument), blancolioni/idle#52 (`Standard_Input` as a default), #397 (console control characters), #331 (`cat`), #339 (`<`), #142 (background jobs)
- **Amended by:** #454, where the line discipline moved from event-input-stream into vga-console, which is now the terminal; and #458, where an end of file at petal's prompt logs out (section 4), because the console now runs `login`, which asks for the next user

## Context

`cat` with no file should copy standard input to standard output, until the user ends the input with Ctrl-D. Under ADR 0009, petal can give a command either a reader that it opened on a file or the job's standard input. Both are `Stream_Reader`s, so `cat` reads either one in the same way (#395).

A `Stream_Reader` has no separate end-of-file operation. A `Read` that returns nothing (`Last < Buffer'First`) is the end of the stream. `cat` stops when it gets one, and Rose's `Ada.Text_IO` sets `At_End` when it gets one.

The console can't produce that `Read` at the moment:

- **Ctrl with a letter produces a NUL.** The keyboard's key table has no entries for Control, so Ctrl-D gives a NUL, which the keyboard echoes and sends on.
- **event-input-stream never answers a `Read` with nothing.** It is the console's line discipline. It collects characters until a line break, then completes the blocked `Read` with the line. If nothing has been typed, it blocks the `Read`.
- **The keyboard echoes every character it produces,** before event-input-stream sees it, so the echo can't depend on what the line discipline does with a character.

Every command that petal launches gets petal's own standard input: petal copies its cap to event-input-stream's `Stream_Reader` into the launch invocation. Ending the input for a command must not end it for petal.

On Unix, Ctrl-D is the terminal's `VEOF` character. The line discipline acts on it, it never appears in the data, and the terminal isn't closed: the next `read` waits for input again.

## Decision

### 1. Ctrl with a letter gives its control character

With Control held, a letter key produces the letter's position modulo 32, so Ctrl-D is EOT (4) and Ctrl-C is ETX (3).

The keyboard stops echoing. Echo moves to event-input-stream, which knows what each character does to the line, and so what should appear on the screen. #397 covers this, and the other control characters.

### 2. The line discipline ends the input on EOT

In cooked mode, event-input-stream acts on EOT, doesn't put it in its buffer, and echoes nothing for it:

- **With an empty line,** the blocked `Read`, or the next `Read` if none is blocked, returns nothing.
- **With part of a line typed,** that part is returned without a line break, as if Enter had been pressed. A second Ctrl-D then ends the input.

In raw mode, EOT is data like any other character.

### 3. End of file is an event, not a state

The stream isn't closed, and nothing is released:

- **event-input-stream keeps one flag,** an end of file that hasn't been read yet. The `Read` that returns nothing clears it, and the `Read` after that blocks for input as usual.
- **The stream is not a `Resource`** (ADR 0010). It lives as long as event-input-stream, and a command that closes its client only deletes its own copy of the cap. Petal's copy still works.
- **What remembers the end of file is the reader.** `At_End` is in the command's own `Ada.Text_IO`, so it ends with the command. Petal's `Ada.Text_IO` never saw that `Read`.

### 4. Petal ignores an end of file at its prompt

A Ctrl-D that no command reads goes to petal. That happens when the user presses Ctrl-D at the prompt, or a command ends before it reads one.

Petal is the console's only shell, and there is no login to return to, so it doesn't log out. It starts a new line and prompts again. `Ada.Text_IO` clears `At_End` when a `Get_Line` reads something, so an interactive reader can carry on after an end of file.

## Consequences

- **`cat` with no file works:** it copies each line as it is entered, and Ctrl-D returns to the prompt (#395).
- **Ctrl-C produces ETX,** in the same place. The line discipline is where ADR 0006 can turn it into `Interrupt`.
- **Input typed ahead goes to the next reader.** Lines typed while nothing is reading wait in event-input-stream, and the next `Read` takes them, whether it comes from a command or from petal. Petal's `Ada.Text_IO` reads everything that is waiting, so lines typed ahead of a command, before petal has read the command, stay in petal's buffer, and petal runs them as commands.
- **Background jobs (#142) can't read the console.** A job and petal would take turns at its input. #142 decides what such a job gets as its standard input.
- **event-input-stream writes to the console.** It needs a `Stream_Writer` cap for it, given to it when it is installed (#397).

## Alternatives considered

- **Pass EOT through as data.** Every reader would have to look for it, and a file can contain the byte 4.
- **Close the stream on Ctrl-D.** Petal would lose its own input, and would need a new stream from event-input-stream for each command.
- **Have petal relay the console to each job,** through a stream of its own that it ends when it reads Ctrl-D. Petal would handle every byte the job reads. Membranes (#119) and pipes (#341) may bring something like this, but end of file doesn't need it.
