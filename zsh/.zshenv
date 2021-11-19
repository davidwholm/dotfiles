if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

typeset -U PATH path
path=("$HOME/.local/bin" "$path[@]")
export PATH

export EDITOR="kcr edit"
export FZF_DEFAULT_OPTS="--multi --layout=reverse --preview-window=down:60%"
export NNN_USE_EDITOR=1
export NNN_TRASH=1
export HISTSIZE=2000
export HISTFILE="$HOME/.history"
export SAVEHIST="$HISTSIZE"
