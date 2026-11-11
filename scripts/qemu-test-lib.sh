# What the boot tests under QEMU (test-clock, test-boot) share.  A test
# sets HERE to the directory of its script, and sources this:
#
#   HERE=$(cd "$(dirname "$0")" && pwd)
#   . "$HERE/qemu-test-lib.sh"
#
# It sets WORK, a new directory for the test's files, DISK, the test's
# disk image, MONITOR, QEMU's monitor, and the first user's name and
# password.  The tests need qemu-system-i386, qemu-img and python3.
#
# WORK is made in ROSE_TEST_DIR, ~/scratch by default, and not in /tmp:
# where /tmp is a small tmpfs, the disk images of a few tests, which a
# failed test keeps, can fill it.

ROSE_TEST_DIR=${ROSE_TEST_DIR:-$HOME/scratch}
mkdir -p "$ROSE_TEST_DIR" || exit 2
WORK=$(mktemp -d "$ROSE_TEST_DIR/rose-test.XXXXXX") || exit 2
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

choose_entry () {
    # choose_entry N: choose entry N of the GRUB menu, counting from 0,
    # as choose_last_entry does, but with Home and then N presses of Down.
    # On the install media with a disk that has Rose, entry 0 boots the
    # disk, and entry 2 installs, erasing it (#836).
    i=0
    while [ $i -lt 24 ]; do
        monitor "sendkey home"
        sleep 0.5
        i=$((i + 1))
    done
    i=0
    while [ $i -lt "$1" ]; do
        monitor "sendkey down"
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
    # type_text TEXT: type TEXT, holding each key while a checkpoint runs
    # (qemu-keys.py, #807)
    ROSE_SERIAL="$WORK/serial" python3 "$HERE/qemu-keys.py" "$MONITOR" "$1"
}

press () {
    # press KEY...: press keys by their QEMU names, such as up or ctrl-s,
    # holding each while a checkpoint runs
    ROSE_SERIAL="$WORK/serial" python3 "$HERE/qemu-keys.py" "$MONITOR" \
        --key "$@"
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

answered=
answer_install () {
    # answer_install SCREEN: answer the install's questions (#456), each
    # once, when it is on the last line of SCREEN, a file of the screen's
    # text: the first user's name and password, the password again, and
    # the enter that reboots the machine.
    # A line of only "?" is the cursor, which fb-text.py can't read, on
    # the line after the question.
    last=$(grep -v -e '^ *$' -e '^ *? *$' "$1" | tail -1)
    case "$answered:$last" in
        :*"User name:"*)
            type_text "$first_user
"
            answered=name ;;
        name:*"Password:"*)
            type_text "$first_password
"
            answered=password ;;
        password:*"Password again:"*)
            type_text "$first_password
"
            answered=again ;;
        again:*"Press enter to reboot."*)
            type_text "
"
            answered=reboot ;;
    esac
}

install_rose () {
    # install_rose ISO: install Rose on a new disk, answering its questions,
    # and wait for the install to finish
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
    if grep -q -e ": FAIL" -e "test failed" "$WORK/install-screen.txt"
    then
        grep -e ": FAIL" -e "test failed" "$WORK/install-screen.txt" \
            | sort -u
        echo "FAIL: one of init's tests failed"
        exit 1
    fi
    echo "init's derive and swap tests: $(sort -u "$WORK/install-screen.txt" \
        | grep -c ': ok') of their checks seen ok"

    # The swap test's last checks follow the release (ADR 0032, #862).
    grep -q "swap test: release the calls: ok" "$WORK/install-screen.txt" \
        || { echo "FAIL: init's swap test was not seen to finish"; exit 1; }

    # The install's run of the self tests is the one that has the cap set
    # server, which petal doesn't give (#825).
    if grep -q "self tests failed" "$WORK/install-screen.txt"; then
        echo "FAIL: the install's self tests failed"
        exit 1
    fi
    cap_sets=$(grep "Cap set test" "$WORK/install-screen.txt" | tail -1)
    echo "$cap_sets"
    case "$cap_sets" in
        *skipped*|*FAIL*|"")
            echo "FAIL: the install's cap set test did not pass"
            exit 1
            ;;
    esac
}

wait_for_install () {
    # wait_for_install SECONDS: wait until the install has finished, which
    # is when the kernel restarts, and keep what init's tests print, which
    # is gone from the screen by then
    waited=0
    : > "$WORK/install-screen.txt"
    answered=
    while [ $waited -lt "$1" ]; do
        screen > "$WORK/install-now.txt"
        grep -e "derive test" -e "swap test" -e "Cap set test.*\(PASS\|FAIL\)" \
            -e "self tests failed" "$WORK/install-now.txt" \
            >> "$WORK/install-screen.txt"
        answer_install "$WORK/install-now.txt"
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

check_keytest () {
    # Run keytest at the petal prompt, which turns line editing off and
    # draws at the corners and the centre of the screen, move its @ with
    # the arrow keys, and check what it drew (#734). Then quit, and check
    # that line editing is back on: petal's command line is echoed again.
    type_text "keytest
"
    wait_for "keytest [0-9]" 30 || fail "keytest did not start"
    for key in up up left; do
        press $key
    done
    sleep 3
    screen > "$WORK/keytest.txt"
    python3 "$HERE/check-keytest.py" "$WORK/keytest.txt" Left -1 -2 \
        || fail "keytest did not draw what it should"
    type_text "q"
    sleep 3
    type_text "echo editing
"
    wait_for "> echo editing" 20 || fail "line editing is not back on"
}

check_kilo () {
    # Edit a new file with kilo (#735): type a line, save it with Ctrl-S,
    # quit with Ctrl-Q, and check the file with cat. Then add a second
    # line and save again. Then type at the start of the file and quit
    # without saving, which takes Ctrl-Q four times, and check that the
    # change was dropped (ADR 0026).
    type_text "kilo notes
"
    wait_for "HELP: Ctrl-S" 30 || fail "kilo did not start"
    type_text "hello from kilo"
    press ctrl-s
    wait_for "16 bytes written" 20 || fail "kilo did not save"
    press ctrl-q
    sleep 3
    type_text "cat notes
"
    wait_for "^hello from kilo" 20 || fail "cat did not show what kilo saved"

    type_text "kilo notes
"
    wait_for "kilo - 1 lines" 30 || fail "kilo did not read the file"
    press down
    type_text "second line"
    press ctrl-s
    wait_for "28 bytes written" 20 || fail "kilo did not save a second line"
    press ctrl-q
    sleep 3
    type_text "cat notes
"
    wait_for "^second line" 20 || fail "cat did not show the second line"

    type_text "kilo notes
"
    wait_for "kilo - 2 lines" 30 || fail "kilo did not read both lines"
    type_text "dropped"
    wait_for "(modified)" 20 || fail "kilo did not mark the change"
    for i in 1 2 3 4; do
        press ctrl-q
        sleep 1
    done
    sleep 3
    type_text "cat notes
"
    wait_for "^second line" 20 || fail "cat did not show the file after kilo"
    screen | grep -q "dropped" && fail "kilo saved a change it should drop"
    screen | grep -q "^hello from kilo" \
        || fail "the file lost its first line"
}
                                                                                                                                                                                                                                                                