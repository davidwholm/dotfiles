#!/usr/bin/env sh

for config in lf emacs kak alacritty tmux nvim sway
do
    mkdir -p ~/.config/"$config"
    stow "$config" -t ~/.config/"$config"
done

stow zsh
stow chromium -t ~/.config
stow wallpapers -t ~/Pictures
