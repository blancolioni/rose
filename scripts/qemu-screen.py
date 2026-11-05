"""Print the text on the VGA screen of a QEMU virtual machine.

usage: qemu-screen.py MONITOR_SOCKET

MONITOR_SOCKET is the path of QEMU's monitor, a Unix socket (-monitor
unix:PATH,server,nowait). The text is what Rose's console has drawn in the
VGA text page, at physical address B8000, one line to each of its 25
rows, without the trailing spaces.  A row with nothing on it is an empty
line, so that a line's position is its row, as in fb-text.py.
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
    # Only the lines of the dump hold cells: the monitor's echo of the
    # command, as it is typed, ends in 0xb800 too.
    cells = [int(cell, 16)
             for line in reply.splitlines()
             if re.match(r"[0-9a-f]+: ", line)
             for cell in re.findall(r"0x([0-9a-f]{4})\b", line)]
    text = "".join(chr(cell & 0xFF) if 32 <= (cell & 0xFF) < 127 else " "
                   for cell in cells)
    for start in range(0, 2000, 80):
        print(text[start:start + 80].rstrip())


main()
