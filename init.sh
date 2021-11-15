#!/bin/sh

for config in foot waybar river emacs kak alacritty tmux nvim sway
do
    mkdir -p ~/.config/"$config"
    stow "$config" -t ~/.config/"$config"
done

stow zsh
stow task
stow scripts -t ~/.local/bin
stow chromium -t ~/.config
stow wallpapers -t ~/Pictures
