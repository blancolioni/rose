#!/bin/sh
# Boot Rose under Bochs.  Serial output goes to logs/bochs-port-1.txt.
# Set BOCHS_DISPLAY=nogui on a headless machine; stop it with pkill bochs.
cd "$(dirname "$0")/.." || exit 1
[ -f images/rose-hd0.img ] || truncate -s 128M images/rose-hd0.img
rm -f images/rose-hd0.img.lock
exec bochs -q -f bochs/bochsrc "display_library: ${BOCHS_DISPLAY:-sdl2}"
