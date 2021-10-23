export LANG="en_US.UTF-8"

tmux_get_session_fzf() {
    sessions=$(tmux list-sessions -F "#{session_name}")
    case $(echo "$sessions" | wc --lines) in
        0)
            echo
            ;;
        1)
            echo "$sessions"
            ;;
        *)
            echo "$sessions" | fzf --prompt "Choose tmux session: "
    esac
}

tmux_attach_session_fzf() {
    session=$(tmux_get_session_fzf)
    ! [ -z "$session" ] && tmux attach-session -t "$session"
}

tmux_kill_session_fzf() {
    session=$(tmux_get_session_fzf)
    ! [ -z "$session" ] && tmux kill-session -t "$session"
}

alias tas="tmux_attach_session_fzf"
alias tks="tmux_kill_session_fzf"
alias tls="tmux list-sessions"
alias tns="tmux new-session -d -s"
alias vi="nvim"
alias vim="nvim"
alias ls="exa"
alias find="fd"
alias grep="rg"
alias cat="bat"

eval "$(starship init zsh)"
