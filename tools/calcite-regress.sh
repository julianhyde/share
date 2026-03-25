#!/bin/bash
# Runs the calcite test suite and emails the results
export ORACLE_HOME=/u01/app/oracle/product/11.2.0/xe
export PATH="${PATH}:${ORACLE_HOME}/bin"

function foo() {
  . /usr/local/bin/jenv ${jdk}
  cd /home/jhyde/regress/${project}
  /usr/local/bin/add-remotes.sh ${project2}
  git fetch origin # don't need '--all'; add-remotes fetched everything else
  if [ "$remote" = hash ]; then
    git checkout -b b-$label $branch
  else
    git fetch $remote
    git checkout -b b-$label $remote/$branch
  fi
  git status
  git log -n 1 --pretty=format:'"%s"' >> $subject
  commit_id=$(git log -n 1 --pretty=format:'%h')
  case $project in
  (mondrian) ;;
  (*)  mvn_flags="-Dmaven.repo.local=$HOME/.m2/other-repository" ;;
  esac
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
  (sqlline)
    case ${jdk} in
    (jdk6)
      # JDK 1.6 needs an earlier maven and hsqldb, and cannot generate docbook
      mvn_flags="${mvn_flags} -Ddocbkx.skip=true -Dhsqldb.version=2.3.4"
      timeout 10m /usr/local/apache-maven-3.2.5/bin/mvn $mvn_flags $flags clean install javadoc:javadoc javadoc:test-javadoc site
      ;;
    (jdk7)
      # JDK 1.7 needs an earlier hsqldb, and cannot generate docbook
      mvn_flags="${mvn_flags} -Ddocbkx.skip=true -Dhsqldb.version=2.3.4"
      timeout 10m mvn $mvn_flags $flags clean install javadoc:javadoc javadoc:test-javadoc site
      ;;
    (*)
      timeout 10m mvn $mvn_flags $flags clean install javadoc:javadoc javadoc:test-javadoc site
      ;;
    esac
    ;;
  (mondrian)
    touch mondrian.properties
    timeout 60m mvn $mvn_flags $flags -Dmondrian.test.db=mysql clean install javadoc:javadoc site
    ;;
  (morel)
    timeout 10m mvn $mvn_flags $flags clean install javadoc:javadoc
    ;;
  (olap4j)
    timeout 20m mvn $mvn_flags $flags -Drat.ignoreErrors -Dmondrian.test.db=mysql clean install javadoc:javadoc javadoc:test-javadoc site
    ;;
  (avatica)
    (
      cd avatica
      timeout 10m ./gradlew $mvn_flags $flags clean build javadocAggregateIncludingTests
    )
    ;;
  (calcite-avatica)
    timeout 10m ./gradlew $mvn_flags $flags clean build javadocAggregateIncludingTests
    ;;
  (calcite|*)
    echo "mvn $mvn_flags -P it,it-oracle $flags clean install javadoc:javadoc javadoc:test-javadoc site"
    #timeout 30m mvn $mvn_flags -P it $flags install # javadoc:javadoc site
    timeout 30m ./gradlew $mvn_flags $flags clean build
    case ${jdk} in
    (*jdk9|*jdk10)
      echo "Skipping javadoc due to JDK bug";;
    (*)
      timeout 30m ./gradlew $mvn_flags $flags javadocAggregateIncludingTests
    esac
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
  echo "For example, the following fetches the latest main branch from the"
  echo "origin remote repository and runs the suite using JDK 1.8:"
  echo
  echo "  calcite-regress.sh jdk8 origin main -DskipTests"
  echo
  echo "Or, to check out a hash and run against JDK 9:"
  echo
  echo "  calcite-regress.sh jdk9 hash abc123"
  echo
  echo "Arguments:"
  echo "--help"
  echo "     Print this help and exit"
  echo "--batch"
  echo "     Submit this task as a batch job"
  echo "jdk"
  echo "     One of jdk6, jdk7, jdk8, jdk9, jdk10, jdk11"
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
  flock /tmp/$project-regress $0 --project $project "$@"
  exit $?
fi

export jdk="$1"
remote="$2"
branch="$3"
shift 3
flags="$*"

case ${jdk} in
(jdk[0-9]|jdk1[0-9]|openjdk[0-9]|openjdk1[0-9]);;
(*) echo "Invalid jdk ${jdk}"; exit 1;;
esac

case ${project} in
(avatica)
  project2=calcite;;
(calcite-avatica|calcite|olap4j|mondrian|morel|sqlline)
  project2=${project};;
(*)
  echo "Unknown project ${project}"
  exit 1;;
esac

if [ ! -d /home/jhyde/regress/${project} ]; then
  cd /home/jhyde/regress
  git clone git@github.com:julianhyde/${project}.git
fi
cd /home/jhyde/regress/${project}
logdir=/home/jhyde/regress/${project}-logs
mkdir -p ${logdir}
label=$(date +%Y%m%d-%H%M%S)
out=${logdir}/regress-${label}.txt
failed=/tmp/failed-${label}.txt
succeeded=/tmp/succeeded-${label}.txt
subject=/tmp/subject-${label}.txt
rm -f $subject $failed $succeeded
touch $subject $failed $succeeded
start_time=$(date +%s)
foo $label > $out 2>&1
end_time=$(date +%s)
duration=$(expr ${end_time} - ${start_time})

D=$(cd $(dirname $(readlink $0)); pwd -P)
awk -v verbose=1 -f ${D}/analyze-regress.awk $out >> $failed

if [ ! -s "$failed" ]; then
  echo "status: 0 fecjd: 00000" >> $succeeded
fi

(
echo "To: julianhyde@gmail.com"
echo "From: julianhyde@gmail.com"
echo "Subject: ${project} regress ${commit_id} ${remote}/${branch} ${jdk} $(awk -v ORS=' ' '{print}' ${succeeded} ${failed} ${subject})" | tee -a $out
echo
if [ -s "$failed" ]; then
  cat $out
else
  echo "Succeeded (jdk: ${jdk}, remote: ${remote}, branch: ${branch}, flags: ${flags}, duration: ${duration}s). Details in ${out}.xz." | tee -a $out
fi
) | /usr/sbin/ssmtp julianhyde@gmail.com
xz $out

# End
