#!/bin/bash
# Create all the git remotes
#
# Create as follows:
#   git remote -v |awk '{printf "git remote add %s %s\n", $1, $2}' | sort -u
function foo() {
  x=
  if test "$3"; then
    x=$3
  else
    x=$1
  fi
  (
  echo Remote $2
  if remoteExists $2; then
    if [ $x = remove ]; then
      git remote remove $2
    else
      : # git fetch $2
    fi
  elif [ $x = remove ]; then
    :
  else
    echo Creating remote ${2}...
    git remote add $2 git@github.com:$2/${x}.git
    git fetch $2
  fi
  ) 2>&1 | awk '{printf "|%s\n",$0}' | sort -k 1 -t'|'
  # sleep 0.1 # rate-limit calls to git
}

function remoteExists {
  grep -qx "$1" < ${remote_list_file}
}

# Check that this file is sorted
diff -u \
     <(grep " foo " $0 | grep -v grep) \
     <(grep " foo " $0 | grep -v grep | env LC_ALL=C sort -f) || exit

remote_list_file=/tmp/remotes_${1}_${$}.txt
git remote > ${remote_list_file}

case "$1" in
(calcite)
  git remote add origin https://gitbox.apache.org/repos/asf/calcite.git
  git remote add ledem git@github.com:julienledem/calcite.git
  git remote add bitbucket git@bitbucket.org:julianhyde/calcite.git

  foo calcite 1m2c3t4
  foo calcite aertoria remove
  foo calcite aleph-zero remove
  foo calcite alexeyroytman
  foo calcite alishaIBM
  foo calcite amaliujia
  foo calcite amansinha100 incubator-calcite
  foo calcite ambition119
  foo calcite amihalik
  foo calcite amoghmargoor incubator-calcite
  foo calcite ankitsinghal
  foo calcite apache incubator-calcite
  foo calcite apilloud
  foo calcite arina-ielchiieva
  foo calcite arunmahadevan
  foo calcite asereda-gs
  foo calcite asolimando
  foo calcite atris
  foo calcite axeisghost
  foo calcite b-slim
  foo calcite batytskyy
  foo calcite bdumon optiq
  foo calcite beikov
  foo calcite benoyantony incubator-optiq
  foo calcite beyond1920
  foo calcite bluejoe2008 remove
  foo calcite ch33hau
  foo calcite chadasa
  foo calcite chandnisingh
  foo calcite chinmaykolhatkar
  foo calcite chrajeshbabu
  foo calcite chunhui-shi incubator-calcite
  foo calcite chunweilei
  foo calcite Contiamo
  foo calcite d4nc00per incubator-calcite
  foo calcite danny0405
  foo calcite darionyaphet
  foo calcite datametica
  foo calcite devth incubator-calcite
  foo calcite diandu remove
  foo calcite dianfu
  foo calcite dingguitao incubator-calcite
  foo calcite dkadams
  foo calcite docete
  foo calcite dremio remove
  foo calcite ebastien
  foo calcite elilevine
  foo calcite eolivelli
  foo calcite ex00
  foo calcite F21
  foo calcite Fokko
  foo calcite Functor10
  foo calcite gabrielreid
  foo calcite gaodayue
  foo calcite georgewfraser
  foo calcite gianm
  foo calcite Gkairi
  foo calcite godfreyhe
  foo calcite gparai
  foo calcite haohui
  foo calcite HeartSaVioR
  foo calcite hequn8128
  foo calcite hortonworks
  foo calcite hsuanyi incubator-calcite
  foo calcite hsyuan
  foo calcite hustfxj
  foo calcite hzyuemeng1
  foo calcite jacques-n incubator-calcite
  foo calcite jaltekruse
  foo calcite JasonMing incubator-calcite
  foo calcite jbalint
  foo calcite jcamachor
  foo calcite jduo
  foo calcite jh3507
  foo calcite jiayuanv127
  foo calcite jinfengni incubator-optiq
  foo calcite joshelser incubator-calcite
  foo calcite jpullok remove
  foo calcite jpullokkaran
  foo calcite JulianFeinauer
  foo calcite julianhyde
  foo calcite jxiang
  foo calcite kaiwangchen
  foo calcite kennknowles
  foo calcite kgyrtkirk
  foo calcite khaitranq calcite-1
  foo calcite kliewkliew
  foo calcite kstirman
  foo calcite KulykRoman incubator-calcite
  foo calcite KurtYoung
  foo calcite Kyligence
  foo calcite lalinsky incubator-calcite
  foo calcite LantaoJin
  foo calcite laurentgo
  foo calcite leesf
  foo calcite LeoWangLZ
  foo calcite Lerm
  foo calcite lfkpoa
  foo calcite lincoln-lil
  foo calcite mapr incubator-calcite
  foo calcite markap14
  foo calcite maryannxue
  foo calcite masayuki038
  foo calcite MGelbana
  foo calcite michaelmior
  foo calcite mikehinchey
  foo calcite milinda
  foo calcite mindcrime
  foo calcite minji-kim
  foo calcite mprudhom remove
  foo calcite msydoron
  foo calcite navis incubator-calcite
  foo calcite ndimiduk incubator-calcite
  foo calcite nishantmonu51
  foo calcite pawelruchaj
  foo calcite pengchengxiong
  foo calcite pengzhiwei2018
  foo calcite psockali
  foo calcite ptrbojko
  foo calcite rajrahul
  foo calcite Ravindar-Munjam
  foo calcite rdsr
  foo calcite remerge
  foo calcite riccardotommasini
  foo calcite risdenk
  foo calcite rmetzger optiq
  foo calcite rtudoran
  foo calcite rubenada
  foo calcite rusanu
  foo calcite samwagg
  foo calcite sbcd90
  foo calcite sergeysimonov
  foo calcite Serhii-Harnyk remove
  foo calcite shmushkis
  foo calcite siddharthteotia
  foo calcite sintown
  foo calcite smola incubator-calcite
  foo calcite snuyanzin
  foo calcite sreev remove
  foo calcite stalbot
  foo calcite sudheeshkatkam incubator-calcite
  foo calcite suez1224
  foo calcite summerleafs
  foo calcite sunjincheng121
  foo calcite tedxu
  foo calcite tmostak
  foo calcite twalthr incubator-calcite
  foo calcite tzolov
  foo calcite VcamX
  foo calcite vdiravka
  foo calcite vijayk
  foo calcite vineetgarg02
  foo calcite vkorukanti
  foo calcite vladimirdolzhenko
  foo calcite vladimirtkach
  foo calcite vlsi incubator-calcite
  foo calcite vrajat incubator-calcite
  foo calcite vvysotskyi
  foo calcite walterddr
  foo calcite wenhuitang
  foo calcite wgorman optiq
  foo calcite wuchong
  foo calcite wxiang7
  foo calcite xhoong incubator-calcite
  foo calcite Xpray
  foo calcite XuQianJin-Stars
  foo calcite yanghua
  foo calcite yanlin-Lynn
  foo calcite yeongwei incubator-calcite
  foo calcite yiming187
  foo calcite yixinglu remove
  foo calcite yssharma incubator-optiq
  foo calcite yuqi1129
  foo calcite zabetak
  foo calcite zhifac
  foo calcite Zhiqiang-He
  foo calcite zhong-j-yu
  foo calcite zhztheplayer
  foo calcite zinking
  ;;

(avatica|calcite-avatica)
  foo calcite-avatica asolimando
  foo calcite-avatica bcogrel
  foo calcite-avatica F21
  foo calcite-avatica haohui
  foo calcite-avatica joshelser
  foo calcite-avatica julianhyde
  foo calcite-avatica lalinsky
  foo calcite-avatica laurentgo
  foo calcite-avatica michaelmior
  foo calcite-avatica risdenk
  foo calcite-avatica snuyanzin
  foo calcite-avatica ssainz
  ;;

(calcite-avatica-go)
  foo calcite-avatica-go Boostport
  foo calcite-avatica-go julianhyde
  ;;

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

(mondrian)
  foo mondrian julianhyde
  ;;

(sqlline)
  foo sqlline arina-ielchiieva
  foo sqlline bpoweski
  foo sqlline fineo-io
  foo sqlline kminder
  foo sqlline liancheng
  foo sqlline masayuki038
  foo sqlline mmattozzi
  foo sqlline mprudhom
  foo sqlline prodonjs
  foo sqlline slankka
  foo sqlline snuyanzin
  foo sqlline swaroopak
  foo sqlline teroz
  foo sqlline vvysotskyi
  ;;

esac

rm ${remote_list_file}

wait

# End add-remotes.sh
