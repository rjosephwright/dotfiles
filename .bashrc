# Do nothing unless shell is interactive.
case $- in
    *i*)
        :
        ;;
    *)
        return
        ;;
esac

# Aliases.
case $(uname) in
    Darwin)
        alias ls='ls -FG'
        ;;
    Linux)
        alias ls='ls -F --color=auto'
        ;;
esac

alias ll='ls -l'
alias lla='ll -a'
alias gerp=grep

# Load asdf.
[ -f ${HOME}/.asdf/asdf.sh ] && . ${HOME}/.asdf/asdf.sh

# Shell completions.
which aws_completer >/dev/null 2>&1 && complete -C aws_completer aws
which az.completion.sh >/dev/null 2>&1 && . $(which az.completion.sh)
which terraform >/dev/null 2>&1 && complete -C $(which terraform) terraform

[ -f ${HOME}/.local/brew/completions/bash/brew ] && . ${HOME}/.local/brew/completions/bash/brew

[ -f ${HOME}/.asdf/completions/asdf.bash ] && . ${HOME}/.asdf/completions/asdf.bash

completion_dir=${HOME}/.local/share/bash-completion
mkdir -p ${completion_dir}
completions=$(ls ${completion_dir})
for completion in ${completions}; do
    . ${completion_dir}/${completion}
done

# SSH agent.
ssh_keys=$(ls ${HOME}/.ssh/id_* | grep -vE '\.pub')

for ssh_key in ${ssh_keys}; do
    [ -f "${ssh_key}" ] || continue
    ssh_key_sha=$(ssh-keygen -lf ${ssh_key})
    if ! ssh-add -l | grep -q "${ssh_key_sha}"; then
        ssh-add ${ssh_key}
    fi
done
