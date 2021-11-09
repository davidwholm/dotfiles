if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

typeset -U PATH path
path=("$HOME/.local/bin" "$path[@]")
export PATH

export EDITOR="kcr edit"
export HISTSIZE=2000
export HISTFILE="$HOME/.history"
export SAVEHIST="$HISTSIZE"
