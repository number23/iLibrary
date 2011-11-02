#! /usr/bin/env zsh

f_green=$'%{\e[0;32m%}'
f_blue=$'%{\e[0;34m%}'
f_red=$'%{\e[0;31m%}'
f_brown=$'%{\e[0;33m%}'
f_cyan=$'%{\e[0;36m%}'
f_no_colour=$'%{\e[0m%}'
PS1="${f_blue}Shell${f_red}:${f_brown}%~ ${f_green}>: ${f_no_colour}"

# .aliasrc.d
for aliasrc_snipplet in ~/.aliasrc.d/S[0-9][0-9]*[^~]; do
	source $aliasrc_snipplet
done

# .zsh.d
for zshrc_snipplet in ~/.zsh.d/S[0-9][0-9]*[^~]; do
	source $zshrc_snipplet
done

export EDITOR=vi
export PAGER=most
export BLOCKSIZE=K

#export LC_CTYPE=en_US.UTF-8
#export LANG=en_US.UTF-8

PATH=.
PATH=$PATH:$HOME/bin
PATH=$PATH:/usr/local/sbin:/usr/local/bin
PATH=$PATH:/usr/sbin:/sbin
PATH=$PATH:/usr/bin:/bin
PATH=$PATH:/usr/X11R6/bin
PATH=$PATH:/usr/X11/bin
export PATH

export U=$HOME
export u=$HOME

# open maxfiles for max os
ulimit -n 2048

## Java
#export JAVA_HOME=/usr/local/jdk-1.7.0
#export CLASSPATH=.:$JAVA_HOME/lib/tools.jar:$JAVA_HOME/lib/dt.jar
#export PATH=$PATH:$JAVA_HOME/bin
#export ANT_HOME=/usr/local/ant
## Clojure
#export CLOJURE_HOME=$HOME/opt/clojure-1.2.0

# Python
#export PYTHONPATH=$HOME/mylibs/lib/Python
export PYTHONSTARTUP=$HOME/.pythonstartup
export PATH=$HOME/opt/pypy/bin:$PATH

# Node.js
export PATH=$HOME/opt/node/bin:$PATH
export PATH=$HOME/node_modules/.bin:$PATH

# proxychains
# cd $HOME/opt/proxychains/bin

#Clozure-cl
export PATH=$HOME/opt/ccl:$PATH

# SBCL
export SBCL_HOME=$HOME/opt/sbcl/lib/sbcl
