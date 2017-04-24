#!/bin/bash
# Create all the git remotes
#
# Create as follows:
#   git remote -v |awk '{printf "git remote add %s %s\n", $1, $2}' | sort -u
function foo {
  x=
  if test "$3"; then
    x=$3
  else
    x=$1
  fi
  (
  echo Remote $2
  if remoteExists $2; then
    :
  else
    echo Creating remote ${2}...
    git remote add $2 git@github.com:$2/${x}.git
  fi
  git fetch $2
  ) 2>&1 | awk '{printf "|%s\n",$0}' | sort -k 1 -t'|' &
  sleep 0.05 # rate-limit calls to git
}

function remoteExists {
  git remote | grep -qx "$1"
}

case "$1" in
(calcite-test-dataset)
  foo calcite-test-dataset b-slim
  foo calcite-test-dataset jcamachor
  foo calcite-test-dataset julianhyde
  foo calcite-test-dataset michaelmior
  foo calcite-test-dataset nishantmonu51
  foo calcite-test-dataset vlsi
  ;;

(drill)
  foo drill cgivre
  ;;

(sqlline)
  foo sqlline bpoweski
  foo sqlline fineo-io
  foo sqlline mmattozzi
  ;;

(mondrian)
  foo mondrian julianhyde
  ;;

(avatica|calcite-avatica)
  foo calcite-avatica joshelser
  foo calcite-avatica julianhyde
  foo calcite-avatica ssainz
  ;;

(calcite-avatica-go)
  foo calcite-avatica-go Boostport
  foo calcite-avatica-go julianhyde
  ;;

(calcite)
  git remote remove bluejoe2008
  git remote remove dremio
  #git remote remove jcamachor
  git remote remove jpullok
  git remote remove sreev
  git remote add origin https://git-wip-us.apache.org/repos/asf/calcite.git
  git remote add ledem git@github.com:julienledem/calcite.git

  #foo calcite aertoria incubator-calcite
  foo calcite aleph-zero incubator-calcite
  foo calcite amansinha100 incubator-calcite
  foo calcite amihalik
  foo calcite amoghmargoor incubator-calcite
  foo calcite ankitsinghal
  foo calcite apache incubator-calcite
  foo calcite arina-ielchiieva
  foo calcite arunmahadevan
  foo calcite atris
  foo calcite axeisghost
  foo calcite batytskyy
  foo calcite bdumon optiq
  foo calcite beikov
  foo calcite benoyantony incubator-optiq
  foo calcite b-slim
  foo calcite chandnisingh
  foo calcite chinmaykolhatkar
  foo calcite chrajeshbabu
  foo calcite Contiamo
  foo calcite ch33hau
  foo calcite d4nc00per incubator-calcite
  foo calcite darionyaphet
  foo calcite devth incubator-calcite
  foo calcite dianfu
  foo calcite dingguitao incubator-calcite
  foo calcite dkadams
  foo calcite docete
  foo calcite ebastien
  foo calcite elilevine
  foo calcite ex00
  foo calcite F21
  foo calcite gabrielreid
  foo calcite gaodayue
  foo calcite georgewfraser
  foo calcite gianm
  foo calcite Gkairi
  foo calcite gparai
  foo calcite haohui
  foo calcite HeartSaVioR
  foo calcite hortonworks
  foo calcite hsuanyi incubator-calcite
  foo calcite hzyuemeng1
  foo calcite jacques-n incubator-calcite
  foo calcite jaltekruse
  foo calcite JasonMing incubator-calcite
  foo calcite jbalint
  foo calcite jcamachor
  foo calcite jinfengni incubator-optiq
  foo calcite joshelser incubator-calcite
  foo calcite jpullokkaran
  foo calcite JulianFeinauer
  foo calcite julianhyde
  foo calcite jxiang
  foo calcite kaiwangchen
  foo calcite kliewkliew
  foo calcite kstirman
  foo calcite KulykRoman incubator-calcite
  foo calcite KurtYoung
  foo calcite Kyligence
  foo calcite lalinsky incubator-calcite
  foo calcite laurentgo
  foo calcite Lerm
  foo calcite mapr incubator-calcite
  foo calcite markap14
  foo calcite maryannxue
  foo calcite michaelmior
  foo calcite mikehinchey
  foo calcite milinda
  foo calcite mindcrime
  foo calcite minji-kim
  foo calcite mprudhom incubator-optiq
  foo calcite navis incubator-calcite
  foo calcite ndimiduk incubator-calcite
  foo calcite nishantmonu51
  foo calcite pawelruchaj
  foo calcite pengchengxiong
  foo calcite remerge
  foo calcite riccardotommasini
  foo calcite risdenk
  foo calcite rmetzger optiq
  foo calcite rtudoran
  foo calcite rusanu
  foo calcite sbcd90
  foo calcite sergeysimonov
  foo calcite Serhii-Harnyk
  foo calcite smola incubator-calcite
  foo calcite sudheeshkatkam incubator-calcite
  foo calcite summerleafs
  foo calcite sunjincheng121
  foo calcite suez1224
  foo calcite tedxu
  foo calcite tmostak
  foo calcite tzolov
  foo calcite VcamX
  foo calcite vineetgarg02
  foo calcite vkorukanti
  foo calcite vlsi incubator-calcite
  foo calcite vrajat incubator-calcite
  foo calcite vvysotskyi
  foo calcite wgorman optiq
  foo calcite wuchong
  foo calcite wxiang7
  foo calcite xhoong incubator-calcite
  foo calcite Xpray
  foo calcite yeongwei incubator-calcite
  foo calcite yiming187
  #foo calcite yixinglu
  foo calcite yuqi1129
  foo calcite yssharma incubator-optiq
  foo calcite zhifac
  foo calcite Zhiqiang-He
  foo calcite zinking
  ;;
esac

wait

# End add-remotes.sh
