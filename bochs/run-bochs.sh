#!/bin/sh
# Boot Rose under Bochs.  Serial output goes to bochs-port-1.txt.
# Set BOCHS_DISPLAY=nogui on a headless machine; stop it with pkill bochs.
cd "$(dirname "$0")/.." || exit 1
[ -f rose-hd0.img ] || truncate -s 128M rose-hd0.img
rm -f rose-hd0.img.lock
exec bochs -q -f bochs/bochsrc "display_library: ${BOCHS_DISPLAY:-sdl2}"
