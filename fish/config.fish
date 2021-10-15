if status is-interactive
    # Commands to run in interactive sessions can go here
end


if status is-login
    if test -n "$DESKTOP_SESSION"
        set -x (gnome-keyring-daemon --start | string split "=")
    end

    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        exec sway
    end
end
