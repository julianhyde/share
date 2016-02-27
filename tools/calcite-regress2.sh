#!/bin/bash
#
# Runs calcite-regress.sh for each of a given list of JDKs

repo=$1
shift
branch=$1
shift
for jdk in $*
do
  echo calcite-regress.sh --batch --exclusive $jdk $repo $branch -Duser.timezone=Europe/Paris
  calcite-regress.sh --batch --exclusive $jdk $repo $branch -Duser.timezone=Europe/Paris
done

# End xxx.sh
