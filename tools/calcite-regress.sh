#!/bin/bash
# Runs the calcite test suite and emails the results

export ORACLE_HOME=/u01/app/oracle/product/11.2.0/xe
export PATH="${PATH}:${ORACLE_HOME}/bin"

function foo() {
  cd /home/jhyde/open1
  . ./env ${jdk}
  cd /home/jhyde/regress/${project}
  git fetch --all
  if [ "$remote" = hash ]; then
    git checkout -b b-$label $branch
  else
    git checkout -b b-$label $remote/$branch
  fi
  git status
  git log -n 1 --pretty=format:'"%s"' >> $subject
  commit_id=$(git log -n 1 --pretty=format:'%h')
  mvn_flags="-Dmaven.repo.local=$HOME/.m2/other-repository"
  (
    cd ${ORACLE_HOME}/jdbc/lib;
    mvn install:install-file \
      $mvn_flags \
      -DgroupId=com.oracle \
      -DartifactId=ojdbc6 \
      -Dversion=11.2.0.2.0 \
      -Dpackaging=jar \
      -Dfile=ojdbc6.jar \
      -DgeneratePom=true
  )
  case ${project} in
  (mondrian)
    touch mondrian.properties
    timeout 60m mvn $mvn_flags $flags -Dmondrian.test.db=mysql clean install javadoc:javadoc site
    ;;
  (olap4j)
    timeout 20m mvn $mvn_flags $flags -Drat.ignoreErrors -Dmondrian.test.db=mysql clean install javadoc:javadoc site
    ;;
  (calcite|*)
    (
      cd avatica
      timeout 10m mvn $mvn_flags $flags clean install javadoc:javadoc site
    )
    echo "mvn $mvn_flags -P it,it-oracle $flags clean install javadoc:javadoc site"
    #timeout 30m mvn $mvn_flags -P it,it-oracle $flags install javadoc:javadoc site
    timeout 30m mvn $mvn_flags $flags clean install javadoc:javadoc site
    ;;
  esac
  status=$?
  echo
  echo status $status
  echo Finished at $(date)
  if [ "$status" -ne 0 ]; then
    echo "status: $status" >> $failed
  fi
}

function usage() {
  remotes="$(cd /home/jhyde/regress/${project}; git remote)"
  echo "Usage:"
  echo "  calcite-regress.sh [ --batch ] [ --project project ] [ --exclusive ] <jdk> <remote> <branch> [flags]"
  echo "  calcite-regress.sh [ --batch ] [ --project project ] [ --exclusive ] <jdk> hash <commit> [flags]"
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

project=calcite
if [ "$1" == --project ]; then
  shift
  project="$1"
  shift
fi

if [ "$1" == --exclusive ]; then
  shift
  # All projects share the same lock file because maven repositories
  # are not thread-safe
  flock /tmp/calcite-regress $0 "$@"
  exit $?
fi

jdk="$1"
remote="$2"
branch="$3"
shift 3
flags="$*"

if [ ! -d /home/jhyde/regress/${project} ]; then
  echo "no directory"
  exit 1
fi

cd /home/jhyde/regress/${project}
mkdir -p logs
label=$(date +%Y%m%d-%H%M%S)
out=$(pwd)/logs/regress-${label}.txt
failed=/tmp/failed-${label}.txt
succeeded=/tmp/succeeded-${label}.txt
subject=/tmp/subject-${label}.txt
rm -f $subject $failed $succeeded
touch $subject $failed $succeeded
foo $label > $out 2>&1

awk '
/^Tests run:/ {f+=$5;e+=$7}
/SIGSEGV/ {++c}
/Tag @link: reference not found/ {++j}
END {
  if (f + e + c + j > 0) {
    printf "fecj: %d:%d:%d:%d\n", f, e, c, j;
  }
}
    ' $out >> $failed

if [ ! -s "$failed" ]; then
  echo "status: 0 fecj: 0000" >> $succeeded
fi

(
echo "To: julianhyde@gmail.com"
echo "From: julianhyde@gmail.com"
echo "Subject: ${project} regress ${commit_id} ${remote}/${branch} ${jdk} $(awk -v ORS=' ' '{print}' ${succeeded} ${failed} ${subject})" | tee -a $out
echo
if [ -s "$failed" ]; then
  cat $out
else
  echo "Succeeded (jdk: ${jdk}, remote: ${remote}, branch: ${branch}, flags: ${flags}). Details in ${out}.xz." | tee -a $out
fi
) | /usr/sbin/ssmtp julianhyde@gmail.com
xz $out

# End
