
export ZSH=/home/ir/.oh-my-zsh

ZSH_THEME="powerlevel9k/powerlevel9k"

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(command_execution_time time)
POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=2

plugins=(
    git
    history-substring-search
    fasd
)

source $ZSH/oh-my-zsh.sh

export PATH="$HOME/bin:$HOME/gocode/bin:/usr/local/go/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
