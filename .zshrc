
export ZSH=/home/ir/.oh-my-zsh

ZSH_THEME="powerlevel9k/powerlevel9k"

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(time)

plugins=(
    git
    history-substring-search
    fasd
)

source $ZSH/oh-my-zsh.sh

export PATH="$HOME/bin:$HOME/gocode/bin:/usr/local/go/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
