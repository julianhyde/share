#!/bin/bash
# Runs the calcite test suite and emails the results

function foo() {
  cd /home/jhyde/open1
  . ./env ${jdk}
  cd /home/jhyde/open1/calcite.3
  git fetch --all
  if [ "$remote" = hash ]; then
    git checkout -b b-$label $branch
  else
    git checkout -b b-$label $remote/$branch
  fi
  git status
  git log -n 1 --pretty=format:'%h "%s"' >> $subject
  mvn_flags="-Dmaven.repo.local=$HOME/.m2/other-repository"
  echo $remote/$branch >> $subject
  echo "mvn $mvn_flags clean && mvn $mvn_flags -Pit $flags install site"
  mvn $mvn_flags clean
  timeout 20m mvn $mvn_flags -Pit $flags install site
  status=$?
  echo
  echo status $status
  echo Finished at $(date)
  if [ "$status" -ne 0 ]; then
    echo "status: $status" >> $failed
  fi
}

function usage() {
  remotes="$(cd /home/jhyde/open1/calcite.3; git remote)"
  echo "Usage:"
  echo "  calcite-regress.sh [ --batch ] <jdk> <remote> <branch> [flags]"
  echo "  calcite-regress.sh [ --batch ] <jdk> hash <commit> [flags]"
  echo "  calcite-regress.sh --help"
  echo
  echo "For example, the following fetches the latest master branch from the"
  echo "origin remote repository and runs the suite using JDK 1.8:"
  echo
  echo "  calcite-regress.sh jdk1.8 origin master -DskipTests"
  echo
  echo "Or, to check out a hash and run against JDK 1.7:"
  echo
  echo "  calcite-regress.sh jdk1.7 hash abc123"
  echo
  echo "Arguments:"
  echo "--help"
  echo "     Print this help and exit"
  echo "--batch"
  echo "     Submit this task as a batch job"
  echo "jdk"
  echo "     One of jdk1.6, jdk1.7, jdk1.8"
  echo "remote"
  echo "      A git remote (one of:" ${remotes} ")"
  echo "branch"
  echo "      A branch within the remote"
  echo "flags"
  echo "     Optional flags to pass to maven command line"
}

if [ $# -lt 3 -o x"$1" = x--help -o x"$1" = x-h ]; then
  usage
  exit 0
fi

if [ "$1" = --batch ]; then
  shift
  echo $0 "$@" | batch
  exit
fi

if [ "$1" == --exclusive ]; then
  shift
  flock /tmp/calcite-regress $0 "$@"
  exit $?
fi

jdk="$1"
remote="$2"
branch="$3"
flags="$4"

cd /home/jhyde/open1/calcite.3
mkdir -p logs
label=$(date +%Y%m%d-%H%M%S)
out=$(pwd)/logs/regress-${label}.txt
failed=/tmp/failed-${label}.txt
subject=/tmp/subject-${label}.txt
rm -f $subject $failed
touch $subject $failed
foo $label > $out 2>&1

awk '
/^Tests run:/ {f+=$5;e+=$7}
END {
  if (f + e > 0) {
    printf "failures: %d errors: %d\n", f, e;
  }
}
    ' $out >> $failed

(
echo "To: julianhyde@gmail.com"
echo "From: julianhyde@gmail.com"
echo "Subject: Calcite regress $(awk -v ORS=' ' '{print}' ${failed} ${subject})"
echo
if [ -s "$failed" ]; then
  cat $out
else
  echo "Succeeded (jdk: ${jdk}, remote: ${remote}, branch: ${branch}, flags: ${flags}). Details in ${out}.xz."
fi
) | /usr/sbin/ssmtp julianhyde@gmail.com
xz $out

# End
