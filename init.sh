#!/usr/bin/env sh

for config in wezterm kak nvim emacs
do
    mkdir -p ~/.config/"$config"
    stow "$config" -t ~/.config/"$config"
done

stow zsh
stow wallpapers -t ~/Pictures
