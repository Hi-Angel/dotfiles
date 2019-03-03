# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/constantine/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="mira"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd/mm/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git themes)

source $ZSH/oh-my-zsh.sh

# User configuration

autoload -U select-word-style
select-word-style bash

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'
zstyle ':completion:*' matcher-list '' \
        'm:{a-z\-}={A-Z\_}' \
        'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
        'r:|?=** m:{a-z\-}={A-Z\_}'

bindkey \^U backward-kill-line
bindkey "^[l" down-case-word

#useful aliases
alias grep1="grep --exclude-dir=\".*\" --exclude=tags --exclude=TAGS"
alias ack='ack --ignore-dir=\\".*\\" --ignore-file=is:tags --ignore-file=is:TAGS --color-lineno=red --color-filename=blue -H'
alias dmesg="dmesg --color=always"
alias gdb="gdb -q"
alias ll="ls -l"
alias ghci512="stack ghci --ghci-options '+RTS -M512m -RTS'"
# alias ghci="stack ghci --ghci-options ''"
alias ghciMath="stack ghci --ghci-options '-ghci-script /home/constantine/.ghciMath'"
# alias ghciMath512="stack ghci --ghci-options '+RTS -M512m -RTS -ghci-script /home/constantine/.ghciMath'"
alias ghc="stack ghc -- "
# export MANPATH="/usr/local/man:$MANPATH"
alias git-head='git checkout $(git log --branches -1 --pretty=format:"%D" | sed "s/.*, //g")'

# You may need to manually set your language environment
export LANG=en_US.UTF-8

export EDITOR='vim'

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

setopt HIST_IGNORE_ALL_DUPS
setopt hist_reduce_blanks
unsetopt share_history

# completion for ninja command
fpath=(~/.zsh/ninja-completion.zsh $fpath)

# sed analog in perl, called like "sed_perl pattern_from pattern_to"
function sed_perl() {
	local from=$1
	local to=$2
	shift 2
	#/usr/bin/vendor_perl/ack -l --print0 "$from" $@ | xargs -r0 perl -i -pe "s\`$from\`$to\`g"
	/usr/bin/vendor_perl/ack -l --print0 "$from" $@ | xargs -r0 perl -Mutf8 -i -CS -pe "s α${from}α${to}αg"
}
