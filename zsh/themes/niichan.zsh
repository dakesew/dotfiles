# Based on the Robby Russel zsh theme, except better suited for my needs

function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

function if_machine {
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    echo "$(hostname) "
fi
}

NAME=""

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
	NAME="%m "
fi

if [ -n "$DOCKER" ]
then
	NAME="docker:%m "
fi

PROMPT='$NAME%{$fg_bold[cyan]%}$(collapse_pwd)%{$reset_color%} $(git_prompt_info)
%{$fg[cyan]%}-%{$fg_bold[cyan]%}-%{$fg_bold[green]%}> %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[magenta]%}(git:%{$fg_bold[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[magenta]%}) %{$fg[yellow]%}X%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[magenta]%}) %{$fg[green]%}O%{$reset_color%}"
