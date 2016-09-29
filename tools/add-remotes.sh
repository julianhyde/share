#!/bin/bash
# Create all the git remotes
#
# Create as follows:
#   git remote -v |awk '{printf "git remote add %s %s\n", $1, $2}' | sort -u
function foo {
  if test "$3"; then
    git remote add $2 git@github.com:$2/$3.git
  else
    git remote add $2 git@github.com:$2/$1.git
  fi
}

case "$1" in
(sqlline)
  foo sqlline fineo-io
  ;;

(calcite)
  git remote remove jpullok
  git remote add origin https://git-wip-us.apache.org/repos/asf/calcite.git
  git remote add ledem git@github.com:julienledem/calcite.git

  foo calcite aertoria incubator-calcite
  foo calcite aleph-zero incubator-calcite
  foo calcite amansinha100 incubator-calcite
  foo calcite amoghmargoor incubator-calcite
  foo calcite apache incubator-calcite
  foo calcite arina-ielchiieva
  foo calcite bdumon optiq
  foo calcite benoyantony incubator-optiq
  foo calcite bluejoe2008
  foo calcite d4nc00per incubator-calcite
  foo calcite devth incubator-calcite
  foo calcite dingguitao incubator-calcite
  foo calcite dremio
  foo calcite ebastien
  foo calcite F21
  foo calcite gabrielreid optiq
  foo calcite gianm
  foo calcite gparai
  foo calcite HeartSaVioR
  foo calcite hortonworks
  foo calcite hsuanyi incubator-calcite
  foo calcite jacques-n incubator-calcite
  foo calcite jaltekruse
  foo calcite JasonMing incubator-calcite
  foo calcite jcamachor
  foo calcite jinfengni incubator-optiq
  foo calcite joshelser incubator-calcite
  foo calcite jpullokkaran
  foo calcite julianhyde
  foo calcite jxiang
  foo calcite lalinsky incubator-calcite
  foo calcite mapr incubator-calcite
  foo calcite maryannxue
  foo calcite michaelmior
  foo calcite mikehinchey
  foo calcite milinda
  foo calcite mindcrime
  foo calcite minji-kim
  foo calcite mprudhom incubator-optiq
  foo calcite navis incubator-calcite
  foo calcite ndimiduk incubator-calcite
  foo calcite remerge
  foo calcite rmetzger optiq
  foo calcite sbcd90
  foo calcite smola incubator-calcite
  foo calcite sreev incubator-optiq
  foo calcite sudheeshkatkam incubator-calcite
  foo calcite tedxu
  foo calcite vkorukanti
  foo calcite vlsi incubator-calcite
  foo calcite vrajat incubator-calcite
  foo calcite wgorman optiq
  foo calcite wuchong
  foo calcite xhoong incubator-calcite
  foo calcite yeongwei incubator-calcite
  foo calcite yiming187
  foo calcite yixinglu
  foo calcite yssharma incubator-optiq
  foo calcite zhifac
  foo calcite zinking
  ;;
esac

# End add-remotes.sh
