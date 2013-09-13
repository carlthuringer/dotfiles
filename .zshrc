# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh
source /opt/boxen/env.sh

# Customize to your needs...
export PATH=/opt/boxen/homebrew/share/npm/bin:node_modules/.bin:/opt/boxen/bin:/opt/boxen/homebrew/bin:/opt/boxen/homebrew/sbin:/usr/local/bin:$USER/bin:$PATH:/usr/bin:/bin:/usr/sbin:/sbin
# NPM bins
export PATH=/opt/boxen/nvm/v0.8.8/bin/jshint:$PATH
# Groupon Git Utils
export PATH=$HOME/Workspace/groupon-git-utils/bin:$PATH
# RBEnv Shims
eval "$(rbenv init -)"


export LC_CTYPE=en_US.UTF-8

#fix iterm2 term reporting not working
export TERM=xterm-256color

# I don't know what this is for
export EDITOR='vim'

PATH=$PATH:$HOME/bin # Add homebin for autossh

function rmb {
  current_branch=$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
  if [ "$current_branch" != "master" ]; then
    echo "WARNING: You are on branch $current_branch, NOT master."
  fi
  echo "Fetching merged branches..."
  git remote prune origin
  remote_branches=$(git branch -r --merged | grep -v '/master$' | grep -v "/$current_branch$")
  local_branches=$(git branch --merged | grep -v 'master$' | grep -v "$current_branch$")
  if [ -z "$remote_branches" ] && [ -z "$local_branches" ]; then
    echo "No existing branches have been merged into $current_branch."
  else
    echo "This will remove the following branches:"
    if [ -n "$remote_branches" ] && [ "$2" = "r" ]; then
      echo "$remote_branches"
    fi
    if [ -n "$local_branches" ]; then
      echo "$local_branches"
    fi
    echo
    if [ "$1" = "y" ] || [ "$1" = "Y" ]; then
      # Remove remote branches
      if [ "$2" = "r" ]; then
        git push origin `git branch -r --merged | grep -v '/master$' | grep -v "/$current_branch$" | sed 's/origin\//:/g' | tr -d '\n'`
      fi
      # Remove local branches
      git branch -d `git branch --merged | grep -v 'master$' | grep -v "$current_branch$" | sed 's/origin\///g' | tr -d '\n'`
    else
      echo "No branches removed."
    fi
  fi
}

function impact {
  git ls-tree --name-only -r HEAD | xargs -n1 git blame --line-porcelain | grep '^author ' | sort | uniq -c | sort -nr
}

# Fix Open With menu duplicates
alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

# Bundle Exec shortcut
alias be='bundle exec $1'

# Rake it all up
alias rakeup='be rake db:drop db:create db:migrate db:test:prepare'

# Set the CPATH for freeimage and ruby_inline problem
export CPATH=/usr/local/include

# Now load all the files in the /shellscripts directory
for file in $HOME/shellscripts/*.sh
do
  echo $file
  source $file
done
