autoload -Uz compinit promptinit
compinit
promptinit

prompt off
zstyle ':completion::complete:*' gain-privileges 1
bindkey -e

setopt hist_ignore_all_dups
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
eval "$(zoxide init zsh)"

alias battery="cat /sys/class/power_supply/BAT0/capacity"
