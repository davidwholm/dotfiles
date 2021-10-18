export LANG="en_US.UTF-8"

tmux_get_session_fzf() {
    tmux list-sessions -F "#{session_name}" | fzf --prompt "Choose tmux session: "
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
alias vi="nvim"
alias vim="nvim"
alias ls="exa"
alias find="fd"
alias grep="rg"
alias cat="bat"

eval "$(starship init zsh)"
