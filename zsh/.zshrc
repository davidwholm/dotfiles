autoload -Uz compinit promptinit
compinit
promptinit

prompt adam1
zstyle ':completion::complete:*' gain-privileges 1
bindkey -e

setopt hist_ignore_all_dups

ZETTEL_DIR="$HOME/zettelkasten"

zettel_generate_id() {
    uuid=$(uuidgen --time)
    zettel=$(echo "$1" | tr ' ' '-')
    echo "$ZETTEL_DIR"/"$uuid"-"$zettel".md
}

zettel_find() {
    zettel=$(fd . "$ZETTEL_DIR" | fzf --print-query --prompt "Choose zettel: ")
    if ! [ "$?" -eq 0 ] && ! [ -z "$zettel" ]
    then
        title="$zettel"
        zettel=$(zettel_generate_id "$zettel")
        echo "# $title" > "$zettel"
    else
        zettel=$(echo "$zettel" | tail -n 1)
    fi
    echo "$zettel"
}

zettel_edit() {
    zettel=$(zettel_find)
    ! [ -z "$zettel" ] && kcr edit $zettel
}

zettel_insert() {
    zettel=$(zettel_find)
    ! [ -z "$zettel" ] && kcr send execute-keys "<esc>i$(basename $zettel)<esc>"
}
