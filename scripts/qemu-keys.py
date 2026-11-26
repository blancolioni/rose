"""Type text on the keyboard of a QEMU virtual machine.

usage: qemu-keys.py MONITOR_SOCKET TEXT
       qemu-keys.py MONITOR_SOCKET --key NAME...

MONITOR_SOCKET is the path of QEMU's monitor, a Unix socket (-monitor
unix:PATH,server,nowait). A line break in TEXT is the Return key. With
--key, each NAME is a key as QEMU's sendkey names it, such as up or
ctrl-s.

Keys are sent at a steady pace, whether or not a checkpoint is running.
Until ADR 0030, a key typed during a checkpoint could be lost, and this
waited for each checkpoint to end (#795, #807). It no longer does, so
the boot tests catch a key that goes missing.
"""

import socket
import sys
import time

NAMES = {" ": "spc", "\n": "ret", "-": "minus", ".": "dot", ",": "comma",
         "_": "shift-minus", ":": "shift-semicolon", "/": "slash",
         ">": "shift-dot", "<": "shift-comma"}


def key(character):
    if character in NAMES:
        return NAMES[character]
    if character.isupper():
        return "shift-" + character.lower()
    return character


def main():
    if len(sys.argv) > 2 and sys.argv[2] == "--key":
        keys = sys.argv[3:]
    else:
        keys = [key(character) for character in sys.argv[2]]
    connection = socket.socket(socket.AF_UNIX)
    connection.connect(sys.argv[1])
    connection.settimeout(1)
    for name in keys:
        connection.sendall(("sendkey " + name + "\n").encode())
        time.sleep(0.15)
    time.sleep(0.5)


main()
