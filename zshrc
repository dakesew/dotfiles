source /usr/share/zsh/scripts/antigen/antigen.zsh
antigen use oh-my-zsh
antigen theme https://github.com/caiogondim/bullet-train-oh-my-zsh-theme bullet-train
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle git
antigen bundle extract
antigen bundle zsh-users/zsh-completions src
antigen bundle command-not-found
antigen apply