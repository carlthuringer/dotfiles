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

# Enable using GnuPG Agent as SSH agent
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

# JWT decoding function
function jwt() {
  for part in 1 2; do
    b64="$(cut -f$part -d. <<< "$1" | tr '_-' '/+')"
    len=${#b64}
    n=$((len % 4))
    if [[ 2 -eq n ]]; then
      b64="${b64}=="
    elif [[ 3 -eq n ]]; then
      b64="${b64}="
    fi
    d="$(openssl enc -base64 -d -A <<< "$b64")"
    python -mjson.tool <<< "$d"
    # don't decode further if this is an encrypted JWT (JWE)
    if [[ 1 -eq part ]] && grep '"enc":' <<< "$d" >/dev/null ; then
        exit 0
    fi
  done
}

# Gifmaker
function dogif() {
  input=$1
  output="${input%.*}.gif"
  width=${2:-800}
  ffmpeg -i "${input}" -filter_complex "[0:v] fps=12,scale=${width}:-1,split [a][b]; [a] palettegen [p]; [b][p] paletteuse" "${output}"
}
