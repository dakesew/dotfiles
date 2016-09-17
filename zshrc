# Save the location of the current completion dump file.
if [ -z "$ZSH_COMPDUMP" ]; then
    ZSH_COMPDUMP="${ZDOTDIR:-${HOME}}/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"
fi

# Load and run compinit
autoload -U compinit
compinit -i -d "${ZSH_COMPDUMP}"

# Use history
SAVEHIST=15000
HISTFILE=~/.zsh_history
setopt append_history extended_history share_history notify

# Superglobs!
setopt extendedglob
unsetopt caseglob

# Load sourcefiles
[ -f ~/.profile ] && source $HOME/.profile

# Detect what platform this is for other scripts
if [[ "$unamestr" == 'Linux' ]]; then
    platform='linux'
elif [[ "$unamestr" == 'Bitrig' ]]; then
    platform='bitrig'
elif [[ "$unamestr" == 'Darwin' ]]; then
    platform='osx'
fi

# report long running command CPU usage
REPORTTIME=60

# Detect what kind of proc we have
proc=`uname -p`

NAME="%n@"

if [ -n "$DOCKER" ]
then
    NAME="%{$fg[blue]%}$NAME""docker:%{$reset_color%}%m "
else
    NAME="$NAME""%m "
fi

if [[ platform != "linux" ]]
then
    NAME="$NAME""($platform) "
fi

function __ret_status {
    echo "%(?:%{$fg[green]%}➜ :%{$fg[red]%}➜ %s)"
}

PROMPT='$NAME%{$fg_bold[green]%}${PWD/#$HOME/~}%{$reset_color%}
%{$fg_bold[gray]%}$(__ret_status) %{$reset_color%}'

# Load extended ZSH aliases and completions
for file in ~/.zsh/*.zsh
do
    source $file
done

# just type '...' to get '../..'
rationalise-dot() {
    local MATCH
    if [[ $LBUFFER =~ '(^|/| |	|'$'\n''|\||;|&)\.\.$' ]]; then
	LBUFFER+=/
	zle self-insert
	zle self-insert
    else
	zle self-insert
    fi
}
zle -N rationalise-dot
bindkey . rationalise-dot
## without this, typing a . aborts incremental history search
bindkey -M isearch . self-insert
#Allow Comments even in interactive shell
setopt interactivecomments

alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g S='| sort'

if type "nvim" > /dev/null
then
    EDITOR=nvim
elif type "vim" > /dev/null
then
    EDITOR=vim
else
    EDITOR=vi
fi

if type "emacsclient" > /dev/null
then
    EDITOR="emacsclient -nw -a '$EDITOR'"
fi
export EDITOR
alias e="$EDITOR"
alias em="emacsclient -nw -a '' "
repl() {
    command="${*}"
    echo 'Started repl for "'$command'"'
    printf "%s> " "$command"
    read -r input
    while [ "$input" != "" ];
    do
	eval "$command $input"
	printf "\n%s> " "$command"
	read -r input
    done
}
if ! pgrep -u $USER ssh-agent > /dev/null; then
    ssh-agent | grep -v echo > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval $(<~/.ssh-agent-thing)
fi
ssh-add -l >/dev/null || alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

