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
 echo -e "\033AnSiTu" $(whoami)
 echo -e "\033AnSiTc" $(pwd)
 echo -e "\033AnSiTh" $(hostname)
 }


for temp in cd pushd popd; do
 alias $temp="eterm-set-cwd $temp"
 done


# set hostname, user, and cwd now
 eterm-reset
fi
