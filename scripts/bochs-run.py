"""Run Bochs with no display, and type on its keyboard.

usage: bochs-run.py serve SOCKET BOCHSRC [SCREEN]
       bochs-run.py type SOCKET TEXT
       bochs-run.py key SOCKET NAME...

serve runs Bochs with BOCHSRC and its "term" display, in a pseudo
terminal of 80 by 25, until Bochs ends or the server is killed, and
takes requests on the Unix socket SOCKET. A test reads the machine's
serial ports, not its screen, but what Bochs draws in the terminal, with
the terminal's escape sequences, is written to SCREEN if it is given,
for a person to read when something has gone wrong.

type types TEXT, in which a line break is the Return key. key presses
each NAME: home, end, up, down, left, right, ret, or ctrl-X for a letter
X. Each waits until the server has typed everything.

Bochs's term display reads the keys from its terminal, and turns the
escape sequences of an xterm into the PC's keys, so this is how a test
types without a display or a person (#1159). QEMU's tests use QEMU's
monitor instead (qemu-keys.py).

If ROSE_SERIAL names the machine's serial log, the server types no key
while a checkpoint is running, as qemu-keys.py doesn't: a key typed
during a checkpoint can be lost (#795, #807).
"""

import fcntl
import os
import pty
import select
import signal
import socket
import struct
import sys
import termios
import time

# What an xterm sends for each key, in the keypad mode that curses turns
# on, and that Bochs's term display knows.
KEYS = {"home": b"\x1bOH", "end": b"\x1bOF", "up": b"\x1bOA",
        "down": b"\x1bOB", "right": b"\x1bOC", "left": b"\x1bOD",
        "ret": b"\r"}

ENTERING = b"Entering checkpoint"
LEAVING = b"Leaving checkpoint"
CHECKPOINT_WAIT = 60
TAIL = 256 * 1024
KEY_DELAY = 0.15


def key_bytes(name):
    if name in KEYS:
        return KEYS[name]
    if name.startswith("ctrl-") and len(name) == 6 and name[5].isalpha():
        return bytes([ord(name[5].lower()) - ord("a") + 1])
    raise ValueError("no key named " + name)


def text_keys(text):
    return [b"\r" if c == "\n" else c.encode() for c in text]


def in_checkpoint(path):
    """Whether the last checkpoint line in the serial log at PATH says one
    has started."""
    try:
        with open(path, "rb") as log:
            log.seek(0, os.SEEK_END)
            size = log.tell()
            log.seek(max(0, size - TAIL))
            text = log.read()
            if ENTERING not in text and LEAVING not in text and size > TAIL:
                log.seek(0)
                text = log.read()
    except OSError:
        return False
    return text.rfind(ENTERING) > text.rfind(LEAVING)


def wait_for_checkpoint(path):
    """Wait, up to CHECKPOINT_WAIT seconds, while a checkpoint runs."""
    if not path:
        return
    deadline = time.monotonic() + CHECKPOINT_WAIT
    while in_checkpoint(path) and time.monotonic() < deadline:
        time.sleep(0.2)


def serve(path, bochsrc, screen_path=None):
    screen = open(screen_path, "ab", buffering=0) if screen_path else None

    def drain():
        """Read what Bochs has drawn, so that it never waits for the
        terminal; False once it has gone."""
        try:
            data = os.read(terminal, 65536)
        except OSError:
            return False
        if screen:
            screen.write(data)
        return bool(data)

    pid, terminal = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "xterm"
        os.execvp("bochs", ["bochs", "-q", "-f", bochsrc,
                            "display_library: term"])
    fcntl.ioctl(terminal, termios.TIOCSWINSZ,
                struct.pack("HHHH", 25, 80, 0, 0))
    os.kill(pid, signal.SIGWINCH)

    def stop(*_):
        # Wait for Bochs to end, so that its disk is free for the next
        # run: it locks the image while it has it open.
        try:
            os.kill(pid, signal.SIGTERM)
            deadline = time.monotonic() + 20
            while (os.waitpid(pid, os.WNOHANG)[0] != pid
                   and time.monotonic() < deadline):
                time.sleep(0.2)
        except OSError:
            pass
        sys.exit(0)

    signal.signal(signal.SIGTERM, stop)
    signal.signal(signal.SIGINT, stop)

    if os.path.exists(path):
        os.unlink(path)
    server = socket.socket(socket.AF_UNIX)
    server.bind(path)
    server.listen(4)
    serial = os.environ.get("ROSE_SERIAL")

    while True:
        ready, _, _ = select.select([terminal, server], [], [], 1)
        if terminal in ready and not drain():
            break
        if server in ready:
            client, _ = server.accept()
            with client:
                request = b""
                while not request.endswith(b"\n"):
                    part = client.recv(4096)
                    if not part:
                        break
                    request += part
                for item in request.rstrip(b"\n").split(b"\0"):
                    if not item:
                        continue
                    wait_for_checkpoint(serial)
                    os.write(terminal, item)
                    deadline = time.monotonic() + KEY_DELAY
                    while time.monotonic() < deadline:
                        r, _, _ = select.select([terminal], [], [], 0.05)
                        if r and not drain():
                            break
                client.sendall(b"done\n")
        if os.waitpid(pid, os.WNOHANG)[0] == pid:
            break
    os.unlink(path)


def send(path, items):
    # Each key's bytes, separated by NUL, which no key has, and ended by a
    # line break, which none has either: Return is a carriage return.
    request = b"\0".join(items) + b"\n"
    client = socket.socket(socket.AF_UNIX)
    # The server makes its socket once Bochs has started.
    deadline = time.monotonic() + 10
    while True:
        try:
            client.connect(path)
            break
        except OSError:
            if time.monotonic() > deadline:
                raise
            time.sleep(0.2)
    client.sendall(request)
    reply = b""
    while not reply.endswith(b"\n"):
        part = client.recv(64)
        if not part:
            break
        reply += part
    client.close()


def main():
    if len(sys.argv) < 4 or sys.argv[1] not in ("serve", "type", "key"):
        sys.exit(__doc__)
    command, path = sys.argv[1], sys.argv[2]
    if command == "serve":
        serve(path, sys.argv[3], sys.argv[4] if len(sys.argv) > 4 else None)
    elif command == "type":
        send(path, text_keys(sys.argv[3]))
    else:
        send(path, [key_bytes(name) for name in sys.argv[3:]])


main()
