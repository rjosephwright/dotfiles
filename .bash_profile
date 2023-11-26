[ $(uname) = Darwin ] && export BASH_SILENCE_DEPRECATION_WARNING=1

EXTRA_PATH=
[ -d ${HOME}/go/bin ]                  && EXTRA_PATH=${EXTRA_PATH}${HOME}/go/bin:
[ -d ${HOME}/.local/brew/bin ]         && EXTRA_PATH=${EXTRA_PATH}${HOME}/.local/brew/bin:
[ -d ${HOME}/.local/npm-packages/bin ] && EXTRA_PATH=${EXTRA_PATH}${HOME}/.local/npm-packages/bin:
[ -d ${HOME}/.local/node/bin ]         && EXTRA_PATH=${EXTRA_PATH}${HOME}/.local/node/bin:
[ -d ${HOME}/.local/bin ]              && EXTRA_PATH=${EXTRA_PATH}${HOME}/.local/bin:
[ -d ${HOME}/.cargo/bin ]              && EXTRA_PATH=${EXTRA_PATH}${HOME}/.cargo/bin:
[ -d ${HOME}/.fly/bin ]                && EXTRA_PATH=${EXTRA_PATH}${HOME}/.fly/bin:
[ -d ${HOME}/.virtualenvs/aws/bin ]    && EXTRA_PATH=${EXTRA_PATH}${HOME}/.virtualenvs/aws/bin:
PATH=${EXTRA_PATH}${PATH}; export PATH

EDITOR=vi; export EDITOR

PS1=': [\u@\h] \w; '; export PS1

[ -f ~/.bashrc ] && . ~/.bashrc
[ -f ~/.asdf/asdf.sh ] && . ~/.asdf/asdf.sh
