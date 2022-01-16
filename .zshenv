# .zshenv
#
# sourced on all shell invocations. generally things that
# aren't needed for an interactive shell, which belong in
# .zshrc.
#
# Path to your oh-my-zsh installation.
export ZSH="/home/troi/.oh-my-zsh"
#
# Would you like to use another custom folder than $ZSH/custom?
export ZSH_CUSTOM="/home/troi/.zshcustom"
#
export MANPATH="/usr/local/man:$MANPATH"
#
# add brew ...
# PATH="$PATH"
export PATH="/home/troi/bin:/home/troi/.local/bin:/home/troi/go/bin:/home/linuxbrew/.linuxbrew/sbin:/home/linuxbrew/.linuxbrew/bin:$PATH"
#
# Umask
#
# /etc/profile sets 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
# umask 027
# Paranoid: neither group nor others have any perms:
# umask 077
umask 033
#
export EDITOR="nvim"
