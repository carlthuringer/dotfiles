# PATH
export PATH="$HOME/bin:$PATH"

# PROMPT
export PS1="\W\\$ "

# RBENV
eval "$(rbenv init -)"

# ALIASES
alias dc="docker-compose"
alias be="bundle exec"
alias resource="source $HOME/.bash_profile"
alias tp="terraform plan"
alias ta="terraform apply"
alias ti="terraform init"
alias tsm="terraform state mv"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# MTG
function mtg {
  filename=$1
  ffmpeg -i ${filename} \
         -r 10 \
         -vf 'scale=650:-1' \
         -f image2pipe \
         -vcodec ppm pipe:1 | \
  convert - \
          +dither \
          -layers Optimize \
          gif:- | \
  gifsicle -O3 \
           --lossy=80 \
           --colors=80 \
           --delay 10 \
           --no-warnings > $(basename ${filename} .mov).gif
}

# Enable using GnuPG Agent as SSH agent
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent
