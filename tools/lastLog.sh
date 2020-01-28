#!/bin/bash

if [ "$1" = --help ]
then
   echo "Usage: lastLog.sh [--help] [--project project] [--grep pattern] [jobCount [lineCount]]"
   echo
   echo "Prints the last 'lineCount' lines of the last 'jobCount' jobs."
   echo "By default, prints the last 20 lines of the last 10 jobs."
   echo "If 'pattern' is specified, only prints files that match that pattern."
   exit 0
fi

project=calcite
if [ "$1" = --project ]
then
    project="$2"
    shift 2
fi

seek=
if [ "$1" = --grep ]
then
    seek="$2"
    shift 2
fi

jobCount=${1:-10}
lineCount=${2:-20}

# Job queue
atq

# Print the last lineCount (default 20) lines of the last jobCount
# (default 10) jobs
case ${project} in
    (calcite|calcite-avatica|morel) logdir=~/regress/${project}-logs;;
    (*) logdir=~/regress/${project}/logs;;
esac
cd ${logdir}
for i in $(ls -r |
               grep '.xz$' |
               if [ "$seek" ]; then head -2000 | xargs xzgrep -l "$seek"; else cat -; fi |
               head -${jobCount})
do
  echo
  echo ::: $i :::
  xzcat $i | tail -${lineCount}
done

# End lastLog.sh

