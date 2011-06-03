#! /usr/local/bin/bash
#
# Shell script to run Oracle SQL commands from within vim
# The results are appended to the end of the buffer
#
# Add To:
#
# .vimrc: the following 4 lines
###  " maps for sqlvim.sh execution
###  map <Leader>sx  zz:'a,.!sqlvim.sh<CR>
###  map <Leader>sc  zz:.!sqlvim.sh<CR>
#
# .bash_profile: the following string
### export ORA_LOGIN=user/password@instance
#

LOGON_AS="/nolog" 
if [[ ! -z ${ORA_LOGIN} ]]
then
    LOGON_AS=${ORA_LOGIN}
fi
SCRIPT=/tmp/t$$.sql
IFS=""
while read line
do 
    echo "${line}"
    echo "${line}" >> $SCRIPT
done
echo "exit" >> $SCRIPT
# make it portable across cygwin and unix
if [[ ! -z ${CYGWIN} ]]
then
    DOSPATH=$(cygpath -d $SCRIPT)
    SCRIPT=$(sed 's/\\/\\\\/g' <<< $DOSPATH) 
fi
sqlplus -S -L ${LOGON_AS} @ $SCRIPT 2>&1 | sed 's/\\r//'
/bin/rm -f $SCRIPT
