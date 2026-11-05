# What the boot tests under QEMU (test-clock, test-boot) share.  A test
# sets HERE to the directory of its script, and sources this:
#
#   HERE=$(cd "$(dirname "$0")" && pwd)
#   . "$HERE/qemu-test-lib.sh"
#
# It sets WORK, a new directory for the test's files, DISK, the test's
# disk image, MONITOR, QEMU's monitor, and the first user's name and
# password.  The tests need qemu-system-i386, qemu-img and python3.

WORK=$(mktemp -d /tmp/rose-test.XXXXXX)
DISK=$WORK/disk.img
MONITOR=$WORK/monitor
first_user=fraser
first_password=rose

fail () {
    echo "FAIL: $1"
    echo "The screen:"
    python3 "$HERE/qemu-screen.py" "$MONITOR" 2> /dev/null | tail -12
    stop_qemu
    exit 1
}

start_qemu () {
    rm -f "$MONITOR"
    qemu-system-i386 -m 32 -display none -serial file:"$WORK/serial" \
        -monitor unix:"$MONITOR",server,nowait -pidfile "$WORK/pid" \
        -drive file="$DISK",index=0,if=ide,format=raw "$@" \
        > "$WORK/qemu.log" 2>&1 &
    sleep 2
}

monitor () {
    # monitor COMMAND: send COMMAND to QEMU's monitor
    python3 - "$MONITOR" "$1" <<'EOF'
import socket, sys, time
c = socket.socket(socket.AF_UNIX)
c.connect(sys.argv[1])
c.sendall((sys.argv[2] + "\n").encode())
time.sleep(0.3)
EOF
}

choose_last_entry () {
    # Choose the last entry of the GRUB menu, which is text mode on the
    # install media and the boot disk: the first is the framebuffer. End
    # is pressed for twelve seconds, which is long enough for the menu to
    # appear and stop its countdown, and does nothing more however often
    # it is pressed.
    i=0
    while [ $i -lt 24 ]; do
        monitor "sendkey end"
        sleep 0.5
        i=$((i + 1))
    done
    monitor "sendkey ret"
}

stop_qemu () {
    [ -f "$WORK/pid" ] && kill "$(cat "$WORK/pid")" 2> /dev/null
    rm -f "$WORK/pid"
    sleep 2
}

screen () {
    python3 "$HERE/qemu-screen.py" "$MONITOR"
}

type_text () {
    python3 "$HERE/qemu-keys.py" "$MONITOR" "$1"
}

wait_for () {
    # wait_for TEXT SECONDS: wait until TEXT is on the screen
    waited=0
    while [ $waited -lt "$2" ]; do
        screen | grep -q -- "$1" && return 0
        sleep 5
        waited=$((waited + 5))
    done
    return 1
}

wait_for_serial () {
    # wait_for_serial TEXT SECONDS: wait until TEXT is in the serial log
    waited=0
    while [ $waited -lt "$2" ]; do
        grep -q -- "$1" "$WORK/serial" 2> /dev/null && return 0
        sleep 5
        waited=$((waited + 5))
    done
    return 1
}

install_rose () {
    # install_rose ISO: install Rose on a new disk, and wait for the
    # install to finish
    [ -f "$1" ] || { echo "no install image: $1"; exit 2; }
    qemu-img create -f raw "$DISK" 6G > /dev/null

    echo "Installing"
    start_qemu -boot d -drive file="$1",if=ide,index=1,media=cdrom
    choose_last_entry

    # When the install is complete, the system restarts from the disk.
    # It says "install complete" on the screen first, but login can clear
    # the screen before the message is seen, so wait for the kernel's
    # restart in the serial log instead.
    wait_for_install 400 || fail "the install did not finish"
    sleep 15
    stop_qemu

    # Init's own tests run during the install, and report on the screen.
    if grep -q -e ": FAIL" -e "derive test failed" "$WORK/install-screen.txt"
    then
        grep -e ": FAIL" -e "derive test failed" "$WORK/install-screen.txt" \
            | sort -u
        echo "FAIL: one of init's tests failed"
        exit 1
    fi
    echo "init's derive test: $(sort -u "$WORK/install-screen.txt" \
        | grep -c ': ok') of its checks seen ok"
}

wait_for_install () {
    # wait_for_install SECONDS: wait until the install has finished, which
    # is when the kernel restarts, and keep what init's tests print, which
    # is gone from the screen by then
    waited=0
    : > "$WORK/install-screen.txt"
    while [ $waited -lt "$1" ]; do
        screen | grep -e "derive test" >> "$WORK/install-screen.txt"
        grep -q -- "kernel: restarting" "$WORK/serial" 2> /dev/null \
            && return 0
        sleep 1
        waited=$((waited + 1))
    done
    return 1
}

log_in () {
    # Log in as the first user, at the login prompt of a boot from the disk
    wait_for "login:" 200 || fail "no login prompt"
    sleep 45
    type_text "$first_user
"
    wait_for "password:" 20 || fail "no password prompt"
    type_text "$first_password
"
    wait_for "petal>" 60 || fail "could not log in"
}
