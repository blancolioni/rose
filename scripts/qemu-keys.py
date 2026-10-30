"""Type text on the keyboard of a QEMU virtual machine.

usage: qemu-keys.py MONITOR_SOCKET TEXT

MONITOR_SOCKET is the path of QEMU's monitor, a Unix socket (-monitor
unix:PATH,server,nowait). A line break in TEXT is the Return key.
"""

import socket
import sys
import time

NAMES = {" ": "spc", "\n": "ret", "-": "minus", ".": "dot",
         "_": "shift-minus", ":": "shift-semicolon", "/": "slash"}


def key(character):
    if character in NAMES:
        return NAMES[character]
    if character.isupper():
        return "shift-" + character.lower()
    return character


def main():
    connection = socket.socket(socket.AF_UNIX)
    connection.connect(sys.argv[1])
    connection.settimeout(1)
    for character in sys.argv[2]:
        connection.sendall(("sendkey " + key(character) + "\n").encode())
        time.sleep(0.15)
    time.sleep(0.5)


main()
