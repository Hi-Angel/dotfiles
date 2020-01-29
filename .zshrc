# the config installation on clean system (from my last attempt):
#    git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
#    git clone https://github.com/robbyrussell/oh-my-zsh ~/.oh-my-zsh
#    chsh -s $(which zsh)
#
# optionally: to migrate bash_history use command below. The grep is to skip
# timestamps in bash format (the command below does not retain them for
# simplicity)
#    cat ~/.bash_history | grep -v -E "^#" | sed 's/^/: 0:0;/' > ~/.zsh_history

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
export ZSH_THEME="mira"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
export ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
export COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
export HIST_STAMPS="dd/mm/yyyy"
export HISTSIZE=10000
export HISTFILESIZE=$HISTSIZE

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
export plugins=(git themes)

source $ZSH/oh-my-zsh.sh

# User configuration

autoload -U select-word-style
select-word-style bash

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'
export ZSH_AUTOSUGGEST_STRATEGY=(history completion) # first history, then what tab would suggest
export ZSH_AUTOSUGGEST_USE_ASYNC=t

zstyle ':completion:*' matcher-list '' \
        'm:{a-z\-}={A-Z\_}' \
        'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
        'r:|?=** m:{a-z\-}={A-Z\_}'

# Note: by default zsh *deletes whole* line with this key
bindkey "^U" backward-kill-line
bindkey "^[l" down-case-word

#useful aliases
alias grep1="grep --exclude-dir=\".*\" --exclude=tags --exclude=TAGS"
alias ack='ack --ignore-dir=\\".*\\" --ignore-dir=build --ignore-file=is:tags --ignore-file=is:TAGS --color-lineno=red --color-filename=blue -H'
alias dmesg="dmesg --color=always"
alias gdb="gdb -q"
alias ll="ls -l"
alias ghc="stack ghc -- "
alias ghci="stack ghci -- "
alias ghci512="stack ghci --ghci-options '+RTS -M512m -RTS'"
alias ghciMath="stack ghci --ghci-options '-ghci-script /home/constantine/.ghciMath'"
# alias ghciMath512="stack ghci --ghci-options '+RTS -M512m -RTS -ghci-script /home/constantine/.ghciMath'"
alias git-head='git checkout $(git log --branches -1 --pretty=format:"%D" | sed "s/.*, //g")'

# You may need to manually set your language environment
export LANG=en_US.UTF-8

export EDITOR='vim'

setopt HIST_IGNORE_ALL_DUPS
setopt hist_reduce_blanks
unsetopt share_history

# completion for ninja command
fpath=(~/.zsh/ninja-completion.zsh $fpath)

# sed analog in perl, called like "sed_perl pattern_from pattern_to [filenames]"
function sed_perl() {
	local from=$1
	local to=$2
	shift 2
	#/usr/bin/vendor_perl/ack -l --print0 "$from" $@ | xargs -r0 perl -i -pe "s\`$from\`$to\`g"
	/usr/bin/vendor_perl/ack -l --print0 "$from" $@ | xargs -r0 perl -Mutf8 -i -CS -pe "s α${from}α${to}αg"
}
alias sp=sed_perl

# deletes lines, called like "del_lines pattern [filenames]"
function del_lines() {
	local pattern=$1
	shift 1
	/usr/bin/vendor_perl/ack -l --print0 "$pattern" $@ | xargs -r0 perl -i -ne 'BEGIN { $re = shift } print if not m/$re/' ${pattern}
}

# adds reviwed-by to n commits
function git_rb() {
    # export arguments, otherwise they're not visible to inline shell executions
	export who=$1
	export mail=$2
	export n=$3
    git rebase HEAD~$n -x 'git commit --amend -m"$(git log --format=%B -n1)$(echo -e \\nReviewed-by: ${who} \<${mail}\>.)"'
}

# rebase-at <action> <comit-ids-and-co>
function rebase-at() {
    local action=$1
    if [[ $(expr substr $action 1 1) == "r" ]] then
       # BUG: when you call "reword", git calls GIT_EDITOR twice, the second one to
       # allow you to edit the commit. But since we set GIT_EDITOR to sed, it simply
       # messes up the text and exits. It's not yet clear how to work around that.
       echo 'reword action is not yet supported, please use "git rebase -i" manually'
       return -1
    fi
    shift 1
    GIT_EDITOR="sed -i -E \"1s/\w+/$action/\"" git rebase -i $@
}

function cs() {
    if [[ $# == 0 ]]; then
        git add -u && git commit -sv
    elif [[ $# == 1 ]]; then
        git add -u && git commit -sm "$1"
    else
        echo "Wrong params number!"
    fi
}

alias rc="git add -u && git rebase --continue"
alias ca="git add -u && git commit --amend -v"
alias cax="git add -u && git commit --amend -v --no-edit"
alias c="git add -u && git commit -v"
alias po="git push origin HEAD"
alias pu="git push upstream HEAD"
alias or="git pull origin   HEAD --rebase"
alias ur="git pull upstream HEAD --rebase"
