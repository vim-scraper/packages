#!/bin/bash
# This script is a commit helper. Usefull to commit using bazaard and vim it shows
# changes on the repo wille you write your commit message
# Nb.: I use ~ in files name because I usualy ignore them (see bzr help ignore)


# filling a file with repo status and change since last commit
scm=bzr
tmpfile_diff=$(mktemp -u "/tmp/commit-diff.XXXXXXXXXXXXXXX")
$scm diff > $tmpfile_diff

# edit the commit message
# better keep it localy
#tmpfile_commit_message=$(mktemp -u "/tmp/commit-message.XXXXXXXXXXXXXXX")
tmpfile_commit_message=.last-commit~

# commit status
tmpfile_commit_status=$(mktemp -u "/tmp/commit-status.XXXXXXXXXXXXXXX")
$scm status >> $tmpfile_commit_status

# load vim
vim -c "e $tmpfile_diff" -c vs -c "e $tmpfile_commit_status" -c 0 -c sp -c "e $tmpfile_commit_message"

# after edition
if [ -f $tmpfile_commit_message ] 
then
  # commit and remove
  $scm commit -F $tmpfile_commit_message || rm $tmpfile_commit_message
else
  # abord commit
  echo "Commit aborded - The file has not been saved"
fi

