"""Type text on the keyboard of a QEMU virtual machine.

usage: qemu-keys.py MONITOR_SOCKET TEXT
       qemu-keys.py MONITOR_SOCKET --key NAME...

MONITOR_SOCKET is the path of QEMU's monitor, a Unix socket (-monitor
unix:PATH,server,nowait). A line break in TEXT is the Return key. With
--key, each NAME is a key as QEMU's sendkey names it, such as up or
ctrl-s.

If ROSE_SERIAL names the machine's serial log, no key is sent while a
checkpoint is running: from the kernel's "Entering checkpoint" to its
"Leaving checkpoint". A key typed during a checkpoint can be lost, since
the keyboard driver is stopped and the PS/2 controller holds only a few
bytes (#795, #807). ADR 0030 would make that safe; until then, the test
waits, as a person would see the console stop.
"""

import os
import socket
import sys
import time

NAMES = {" ": "spc", "\n": "ret", "-": "minus", ".": "dot", ",": "comma",
         "_": "shift-minus", ":": "shift-semicolon", "/": "slash"}

ENTERING = b"Entering checkpoint"
LEAVING = b"Leaving checkpoint"
CHECKPOINT_WAIT = 60
TAIL = 256 * 1024


def key(character):
    if character in NAMES:
        return NAMES[character]
    if character.isupper():
        return "shift-" + character.lower()
    return character


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


def main():
    if len(sys.argv) > 2 and sys.argv[2] == "--key":
        keys = sys.argv[3:]
    else:
        keys = [key(character) for character in sys.argv[2]]
    serial = os.environ.get("ROSE_SERIAL")
    connection = socket.socket(socket.AF_UNIX)
    connection.connect(sys.argv[1])
    connection.settimeout(1)
    for name in keys:
        wait_for_checkpoint(serial)
        connection.sendall(("sendkey " + name + "\n").encode())
        time.sleep(0.15)
    time.sleep(0.5)


main()
