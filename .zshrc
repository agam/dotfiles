# Step zero: load antigen
source /usr/share/zsh-antigen/antigen.zsh

## One option for Nix
## source $(nix eval --raw 'nixpkgs.antigen')/share/antigen/antigen.zsh

# Step 1: Layer on oh-my-zsh
antigen use oh-my-zsh

# Step 2: load a theme
antigen theme https://github.com/denysdovhan/spaceship-zsh-theme spaceship

# Pre-requisite: `git clone https://github.com/zsh-users/zsh-autosuggestions` into `~/.zsh`

# Step 3: a few bundles
antigen bundle git
antigen bundle lein
antigen bundle z
antigen bundle fzf
antigen bundle zsh-users/zsh-syntax-highlighting

# Step 4: apply bundles
antigen apply

# Note: path is set in ~/.zprofile
# export PATH="$HOME/.cargo/bin::$HOME/software/bin:$PATH"

# Step 5: aliases
alias l="exa -lah"
