export LANG="en_US.UTF-8"
export LC_COLLATE="C"

tmux_attach_session_fzf() {
    session=$(tmux list-sessions -F "#{session_name}" | fzf --prompt "Choose tmux session: ")
    ! [ -z "$session" ] && tmux attach-session -t "$session"
}

alias tas="tmux_attach_session_fzf"
alias tls="tmux list-sessions"
alias vi="nvim"
alias vim="nvim"
alias ls="exa"
alias find="fd"
alias grep="rg"
alias cat="bat"

eval "$(starship init zsh)"
