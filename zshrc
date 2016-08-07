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
setopt sharehistory
setopt extendedhistory

# Superglobs!
setopt extendedglob
unsetopt caseglob

# Load sourcefiles
[ -f ~/.profile ] && source $HOME/.profile

# Detect what platform this is for other scripts
platform='unknown'
unamestr=`uname`
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

# Vim is love, vim is life
export EDITOR=vim

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
alias em="emacsclient -nw -a '' "
alias e="emacsclient -nw -a 'vi' "
if ! pgrep -u $USER ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval $(<~/.ssh-agent-thing)
fi
ssh-add -l >/dev/null || alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
