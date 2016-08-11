#!/bin/bash

# Simple wrapper to use patdiff with git, as external diff tool
# You may enable it with
# $ export GIT_EXTERNAL_DIFF=$(where patdiff-git-wrapper.sh)
# OR
# $ git config --global diff.external $(where patdiff-git-wrapper.sh)

# Script called with these parameters:
# path old-file old-hex old-mode new-file new-hex new-mode

# Try to give as much information as git diff default outpout
alt_new="new/$1 $7 ${6:0:9}"
alt_old="old/$1 $4 ${3:0:9}"

"/home/lwzukw/.opam/4.03.0/bin/patdiff" "$2" "$5" "-alt-new" "$alt_new" \
  "-alt-old" "$alt_old"
exit_code=$!
# Patdiff follows diff standard for exit codes, according to the manual. We
# overrides these since git seems to expect 0 in case of success.
# 0: same
# 1: different
# 2: error
if [[ $exit_code -le 1 ]]; then
  exit 0
else
  echo "Exit $exit_code"
  exit $exit_code
fi

