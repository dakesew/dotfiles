#!/usr/bin/env zsh
fpath=( "/home/david/.zsh" $fpath )
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch notify
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/david/.zshrc'

autoload -Uz compinit
compinit


# End of lines added by compinstall
###############################################################################
#User code

autoload -U promptinit && promptinit

prompt pure

alias ls='ls --color=auto'
#Stuff I like from the grml zsh config
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

# print hex value of a number
hex() {
    emulate -L zsh
    if [[ -n "$1" ]]; then
        printf "%x\n" $1
    else
        print 'Usage: hex <number-to-convert>'
        return 1
    fi
}

setopt append_history
setopt extended_history
setopt share_history
setopt notify
setopt completeinword


# support colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
## Emacs: ansi-term + tramp integration
## in ansi-term, ssh to this remote computer, can do C-x C-f and find file in REMOTE working directory
## http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
#Emacs ansi-term directory tracking
# track directory, username, and cwd for remote logons
if [ $TERM = eterm-color ]; then
 function eterm-set-cwd {
 $@
 echo -e "\033AnSiTc" $(pwd)
 }



# set hostname, user, and cwd
 function eterm-reset {
# echo -e "\033AnSiTu" $(whoami)
 echo -e "\033AnSiTc" $(pwd)
# echo -e "\033AnSiTh" $(hostname)
 }


for temp in cd pushd popd; do
 alias $temp="eterm-set-cwd $temp"
 done


# set hostname, user, and cwd now
 eterm-reset
fi

if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi
