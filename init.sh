#!/bin/sh

for config in zathura mako kanshi foot waybar river kak tmux
do
    mkdir -p ~/.config/"$config"
    stow "$config" -t ~/.config/"$config"
done

stow zsh
stow scripts -t ~/.local/bin
stow chromium -t ~/.config
stow wallpapers -t ~/Pictures
