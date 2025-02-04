export PATH="$PATH:$HOME/bin:$HOME/scripts:$HOME/.local/bin:/usr/sbin:/snap/bin/"
export DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1
export STARSHIP_CONFIG=~/.config/starship/starship.toml
# XDG_DATA_DIRS=/usr/share/:/usr/local/share/:/var/lib/snapd/desktop/

# Created by newuser for 5.8
autoload -U colors && colors
source "$HOME/.cargo/env"
eval "$(starship init zsh)"

# History
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=~/.zsh_history

# Tab completion
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.


#### ALIASES ####

alias l="exa -alh --icons --group-directories-first"
alias f="feh -F"
alias p="qpdfview"
alias rr="open *.Rproj"
alias f="feh -F"
alias rb="R CMD INSTALL --preclean --no-multiarch --with-keep.source"
alias cat="batcat --theme='OneHalfLight'"
alias v="nvim"
alias vv="nvim ."
alias sq="source /home/arekkas/src/qtile/venv/bin/activate && startx"

# ---- Git ----
alias gg="git status --short"
alias ga="git add"
alias gaa="git add ."
alias gcm="git commit -m"
alias gp="git push"
alias gpo="git push origin"
alias guo="git pull origin"
alias gco="git checkout"
alias gl="git log -n 5 --oneline"

# ---- Projects ----
alias her="cd ~/Documents/projects/Erevnw-Kainotomw/heracles"

# ---- Django ----
alias pmmm="python3 ./manage.py makemigrations"
alias pmm="python3 ./manage.py migrate"
alias pmcs="python3 ./manage.py createsuperuser"
alias pmrs="python3 ./manage.py runserver"

# ---- tmux ----
alias tt='tmux new-session -s'
alias tat='tmux a -t'

# ---- quarto ----
alias qr="quarto render"
alias qp="quarto publish gh-pages"

##### END ALIASES ####

# vi mode
bindkey -v
bindkey "^H" backward-delete-char
bindkey "^?" backward-delete-char
export KEYTIMEOUT=1

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

source Documents/git/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source Documents/git/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh 2>/dev/null
