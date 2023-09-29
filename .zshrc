# the config installation on clean system (from my last attempt):
#    git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
#    git clone https://github.com/robbyrussell/oh-my-zsh ~/.oh-my-zsh
#    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh/zsh-syntax-highlighting
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
export DISABLE_AUTO_UPDATE="true"

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
export HIST_STAMPS="%d.%m.%y %T"
export HISTSIZE=100000
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
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=12'
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
alias ack='ack "--ignore-dir=match:^\." --ignore-dir=build --ignore-file=is:tags --ignore-file=is:TAGS --color-lineno=red --color-filename=blue -H'
alias dmesg="dmesg --color=always"
alias gdb="gdb -q"
alias ll="ls -l"
alias ghc="stack ghc -- "
alias ghci="stack ghci -- "
alias ghci512="stack ghci --ghci-options '+RTS -M512m -RTS'"
alias ghciMath="stack ghci --ghci-options '-ghci-script /home/constantine/.ghciMath'"
# alias ghciMath512="stack ghci --ghci-options '+RTS -M512m -RTS -ghci-script /home/constantine/.ghciMath'"
alias git-head='git checkout $(git log --branches -1 --pretty=format:"%D" | sed "s/.*, //g")'

# colorizing "cat"
alias ccat='pygmentize -g'

# You may need to manually set your language environment
export LANG=en_US.UTF-8

export EDITOR='vim'

setopt HIST_IGNORE_ALL_DUPS
setopt hist_reduce_blanks
setopt INC_APPEND_HISTORY # append history during the session, but do not load the file
unsetopt sharehistory # it's set elsewhere (perhaps oh-my-zsh?), disable it.

# completion for ninja command
fpath=(~/.zsh/ninja-completion.zsh $fpath)

## path to `ack` varies between systems, so acquire it explicitly
ack_binary=$(whereis -b ack | awk '{print $2}')

# sed analog in perl, called like "sed_perl pattern_from pattern_to [filenames]"
function sed_perl() {
	local from=$1
	local to=$2
	shift 2
	$ack_binary -l --print0 "$from" "$@" | xargs -r0 perl -Mutf8 -i -CS -pe "s α${from}α${to}αg"
}
alias sp=sed_perl

# deletes lines, called like "del_lines pattern [filenames]"
function del_lines() {
	local pattern=$1
	shift 1
	$ack_binary -l --print0 "$pattern" "$@" | xargs -r0 perl -i -ne 'BEGIN { $re = shift } print if not m/$re/' ${pattern}
}

# Logs while a command is running. Usage example `log_with 'journalctl -f > 1' 'ping localhost'`.
function log_with() (
	local log_cmd=$1
	local exec_cmd=$2
    sh -c "$log_cmd" &
    local log_pid=$!
    trap 'kill "$log_pid"' EXIT
    eval "$exec_cmd"
)

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
    shift 1
    GIT_SEQUENCE_EDITOR="sed -i -E \"1s/\w+/$action/\"" git rebase -i "$@"
}

function cs() {
    if [[ $# == 0 ]]; then
        git add -u && git commit -sv
    elif [[ $# == 1 ]]; then
        git add "$1" && git commit -sv
    else
        echo "Wrong params number!"
    fi
}

function c() {
    if [[ $# == 0 ]]; then
        git add -u && git commit -v
    elif [[ $# == 1 ]]; then
        git add "$1" && git commit -v
    else
        echo "Wrong params number!"
    fi
}

function co_prev() {
    git checkout HEAD^ "$@" && git reset
}

function newbranch() {
    if   [[ $# == 1 ]]; then
        git checkout --no-track -b $1 upstream/master
    elif [[ $# == 2 ]]; then
        git checkout --no-track -b $1 $2
    else
        echo "Wrong params number!"
    fi
}

# Define autocompletion for newbranch
_newbranch_comp() {
    _arguments \
        '1: :->branch' \
        '2: :->source'

    case $state in
        branch)
            # nothing
            ;;
        source)
            compadd $(git branch -r | awk '{print $1}')
            ;;
    esac
}

compdef _newbranch_comp newbranch

function p() {
    if [ -z $(git rev-parse --abbrev-ref --symbolic-full-name @{u}) ]; then
        echo "INFO: setting the branch to track origin"
        git push -u origin HEAD "$@"
    else
        git push "$@"
    fi
}

function ct() (
    cd "$(git rev-parse --show-toplevel)"
    if [ -f .git/CHERRY_PICK_HEAD ]; then
        git add -u && GIT_EDITOR=true git cherry-pick --continue
    elif [ -f .git/REBASE_HEAD ]; then
        git add -u && GIT_EDITOR=true git rebase --continue
    elif [ -d .git/rebase-apply ]; then
        git add -u && GIT_EDITOR=true git am --continue
    else
        echo "No operation in progress that is suitable for continue"
        return 1
    fi
 )

function ab() (
    cd "$(git rev-parse --show-toplevel)"
    if [ -f .git/CHERRY_PICK_HEAD ]; then
        git cherry-pick --abort
    elif [ -f .git/REBASE_HEAD ] || [ -d .git/rebase-merge ]; then
        git rebase --abort
    elif [ -d .git/rebase-apply ]; then
        git am --abort
    else
        echo "Conflict due to an unknown operation"
        return 1
    fi
 )

# show the commit whose application triggered the conflict
function conflicted_commit() (
    cd "$(git rev-parse --show-toplevel)"
    git log -1 -p --stat $(awk 'END{print $2}' .git/rebase-merge/done)
 )

# git-infix: find file in the repo containing the word
function fnfx () {
    git ls-files ":/*$1*"
}

# run interactive git-rebase over the commits of the current branch
function ri () {
    target_branch=$([ -z $1 ] && echo upstream/master || echo "$1")
    git rebase -i HEAD~$(l ${target_branch}.. | wc -l)
}

alias gd="git diff -p --stat"
alias rc="git add -u && GIT_EDITOR=true git rebase --continue"
alias ca="git add -u && git commit --amend -v"
alias cax="git add -u && git commit --amend -v --no-edit"
alias or="git pull origin   HEAD --rebase"
alias ur="git pull upstream HEAD --rebase"
alias um="git fetch upstream master && git rebase upstream/master"
alias co="git checkout"
alias l="git l"
alias lp="git log -p --stat"
alias gg="git --no-pager grep -In"
alias ggf="git --no-pager grep -In --files-with-matches"
alias ip="ip -c"

# I often find useful being able to peek at command timestamps in the scrollback
PROMPT='%{$fg[yellow]%}[%D{%d.%m.%Y-%H:%M:%S}] '$PROMPT

# do not "eat" control sequences while a command being executed. This is useful when
# I have a command running, and I want to press, say, ^R to enable reverse-search,
# type something into it, and press RET to make it execute. What will happen is that
# when a command finishes, everything I typed will get executed. Usually, ^R isn't
# passed, only the usual text. This setting makes ^R to get passed as well.
stty rprnt ''

# Re-execute prev. command and insert its output at the caret. Mostly useful for when
# a command returned single line that you want to re-use, like cd into the directory
# and the like.
function _insert-last-command-output () {
    local hist=$history[$((HISTCMD-1))] REPLY

    autoload -U read-from-minibuffer
    LBUFFER+=${(j: :)${(qf)"$(eval $hist 2> /dev/null)"}}
}
bindkey "^X^H"    _insert-last-command-output
zle -N _insert-last-command-output

# NOTE: the pluging documentation states it should be sourced at the end of .zshrc
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
