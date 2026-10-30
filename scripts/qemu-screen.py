"""Print the text on the VGA screen of a QEMU virtual machine.

usage: qemu-screen.py MONITOR_SOCKET

MONITOR_SOCKET is the path of QEMU's monitor, a Unix socket (-monitor
unix:PATH,server,nowait). The text is what Rose's console has drawn in the
VGA text page, at physical address B8000, one line to a row, without the
trailing spaces.
"""

import re
import socket
import sys
import time


def command(connection, text):
    connection.sendall((text + "\n").encode())
    time.sleep(1.5)
    reply = b""
    try:
        while True:
            data = connection.recv(65536)
            if not data:
                break
            reply += data
    except socket.timeout:
        pass
    return reply.decode(errors="replace")


def main():
    connection = socket.socket(socket.AF_UNIX)
    connection.connect(sys.argv[1])
    connection.settimeout(2)
    command(connection, "")
    reply = command(connection, "xp /2000hx 0xb8000")
    cells = [int(cell, 16)
             for cell in re.findall(r"0x([0-9a-f]{4})\b",
                                    reply.split("xp", 1)[1])]
    text = "".join(chr(cell & 0xFF) if 32 <= (cell & 0xFF) < 127 else " "
                   for cell in cells)
    for start in range(0, 2000, 80):
        line = text[start:start + 80].rstrip()
        if line:
            print(line)


main()
