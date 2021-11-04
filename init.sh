#!/usr/bin/env sh

for config in kitty nvim emacs
do
    mkdir -p ~/.config/"$config"
    stow "$config" -t ~/.config/"$config"
done

stow zsh
stow wallpapers -t ~/Pictures
