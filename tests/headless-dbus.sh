#!/usr/bin/env bash
#
# Run XMonad.River.DBus against a real compositor and a real bus, and
# assert on the signals a panel client sees.
#
# Usage: tests/headless-dbus.sh [seconds]
#
# tests/headless-river.sh proves the wire stack; this proves the panel
# channel end to end: the service owns its name, emits the initial
# WorkspacesChanged/WindowsChanged/FocusChanged burst, and a
# SwitchWorkspace method call flips the current workspace in the next
# signal.  Everything runs inside dbus-run-session, so no live session
# bus is touched and nothing outlives the script.

set -uo pipefail

cd "$(dirname "$0")/.."

DURATION=${1:-12}

if ! command -v river >/dev/null; then
    echo "headless-dbus: river is not installed; skipping" >&2
    exit 77
fi
if ! command -v dbus-run-session >/dev/null; then
    echo "headless-dbus: dbus-run-session is not installed; skipping" >&2
    exit 77
fi
if ! command -v busctl >/dev/null; then
    echo "headless-dbus: busctl is not installed; skipping" >&2
    exit 77
fi

WM=${XMONAD_RIVER_WM:-}
[ -n "$WM" ] || WM=$(find dist-newstyle -name river-dbus-spec -type f -perm -u+x 2>/dev/null | head -1)
if [ -z "$WM" ]; then
    echo "headless-dbus: no river-dbus-spec build found; run" >&2
    echo "  cabal build exe:river-dbus-spec" >&2
    exit 1
fi
case "$WM" in /*) ;; *) WM=$PWD/$WM ;; esac

RT=$(mktemp -d /tmp/xrd.XXXXXX)
chmod 700 "$RT"
MON=$RT/monitor.log
CALLS=$RT/calls.log
trap 'rm -rf "$RT"' EXIT

# A client to open a window with, so WindowsChanged/FocusChanged have
# content.  Any Wayland client will do.
CLIENT=""
for c in foot alacritty kitty weston-terminal; do
    command -v "$c" >/dev/null && { CLIENT=$c; break; }
done

cat > "$RT/init.sh" <<EOF
#!/bin/sh
"$WM" &
sleep 2
${CLIENT:+$CLIENT >/dev/null 2>&1 &}
sleep $DURATION
EOF
chmod +x "$RT/init.sh"

timeout $((DURATION + 20)) dbus-run-session -- sh -c '
  RT='"$RT"'
  DURATION='"$DURATION"'
  # unfiltered monitor from the start: the initial signal burst must
  # not race a monitor that attaches only after the name is owned
  busctl --user monitor > "$RT/monitor.log" 2>&1 &
  MON_PID=$!
  river -log-level error -no-xwayland -c "$RT/init.sh" &
  RIVER_PID=$!
  for i in $(seq 1 100); do
      busctl --user list 2>/dev/null | grep -q org.xmonad.WM && break
      sleep 0.1
  done
  sleep 2
  busctl --user call org.xmonad.WM /org/xmonad/WM org.xmonad.WM SwitchWorkspace s beta \
      >> "$RT/calls.log" 2>&1
  sleep 2
  busctl --user call org.xmonad.WM /org/xmonad/WM org.xmonad.WM SwitchWorkspace s alpha \
      >> "$RT/calls.log" 2>&1
  sleep 2
  kill "$MON_PID" "$RIVER_PID" 2>/dev/null
  wait "$RIVER_PID" 2>/dev/null
'

status=0
report() {
    if [ "$2" = ok ]; then printf '  PASS  %s\n' "$1"
    else printf '  FAIL  %s\n' "$1" >&2; status=1
    fi
}

ws_signals=$(grep -c 'Member=WorkspacesChanged' "$MON" 2>/dev/null)
ws_signals=${ws_signals:-0}

echo
echo "headless-dbus: results"

[ "$ws_signals" -ge 1 ] \
    && report "WorkspacesChanged emitted ($ws_signals)" ok \
    || report "WorkspacesChanged emitted (got 0)" no

if [ -n "$CLIENT" ]; then
    grep -q 'Member=WindowsChanged' "$MON" \
        && report "WindowsChanged emitted (client: $CLIENT)" ok \
        || report "WindowsChanged emitted (client: $CLIENT)" no

    grep -q 'Member=FocusChanged' "$MON" \
        && report "FocusChanged emitted" ok \
        || report "FocusChanged emitted" no
else
    echo "  SKIP  WindowsChanged/FocusChanged (no Wayland client found)" >&2
fi

# The assertion that matters: after SwitchWorkspace beta, a signal
# arrives naming beta as current.  busctl monitor prints each signal
# followed by its body; a workspace entry looks like
#   STRING "beta";
#   VARIANT "(bb)" { BOOLEAN true; BOOLEAN false; };
# so "STRING \"beta\"" followed within a few lines by "true" is the
# current-workspace entry.  The initial state has alpha current, so
# finding beta current at all proves the method call took effect.
if grep -A6 'STRING "beta"' "$MON" | grep -q 'BOOLEAN true'; then
    report "SwitchWorkspace flipped the current workspace" ok
else
    report "SwitchWorkspace flipped the current workspace" no
fi

if [ -s "$CALLS" ] && grep -qi 'error' "$CALLS"; then
    report "SwitchWorkspace calls returned cleanly" no
    head -5 "$CALLS" >&2
else
    report "SwitchWorkspace calls returned cleanly" ok
fi

if [ "$status" -ne 0 ]; then
    echo >&2
    echo "monitor log:" >&2
    sed 's/^/  | /' "$MON" | head -40 >&2
fi
exit "$status"
