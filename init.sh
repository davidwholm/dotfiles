#!/usr/bin/env sh

for config in alacritty nvim sway tmux
do
    mkdir -p ~/.config/"$config"
    stow "$config" -t ~/.config/"$config"
done

stow zsh
stow starship -t ~/.config
