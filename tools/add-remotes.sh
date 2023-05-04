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
     <(grep " foo " $0 | grep -v grep | env LC_ALL=C sort -f -u) || exit

remote_list_file=/tmp/remotes_${1}_${$}.txt
git remote > ${remote_list_file}

# If project is not specified as an argument, choose project based on
# current directory.
if [ "$1" ]; then
  project="$1"
else
  case $(pwd) in
  (*/avatica-go*|*/calcite-avatica-go*) project=avatica-go;;
  (*/avatica*|*/calcite-avatica*) project=avatica;;
  (*/calcite-test-dataset*) project=calcite-test-dataset;;
  (*/calcite*) project=calcite;;
  (*/drill*) project=drill;;
  (*/mondrian*) project=mondrian;;
  (*/morel*) project=morel;;
  (*/quidem*) project=quidem;;
  (*/share*) project=share;;
  (*/sql-logic-test*) project=sql-logic-test;;
  (*/sqlline*) project=sqlline;;
  (*) echo "Unknown project, and cannot deduce from pwd, $(pwd)"; exit 1;;
  esac
fi

case "$project" in
(calcite)
  git remote add origin https://gitbox.apache.org/repos/asf/calcite.git
  git remote add ledem git@github.com:julienledem/calcite.git
  git remote add bitbucket git@bitbucket.org:julianhyde/calcite.git

  foo calcite 1m2c3t4
  foo calcite a-rafay
  foo calcite Aaaaaaron
  foo calcite abhishek-das-gupta
  foo calcite aertoria remove
  foo calcite agajst
  foo calcite akshayabd
  foo calcite aleph-zero remove
  foo calcite alexeyroytman
  foo calcite alishaIBM
  foo calcite amaliujia
  foo calcite amansinha100 incubator-calcite
  foo calcite ambition119
  foo calcite amihalik
  foo calcite amoghmargoor incubator-calcite
  foo calcite anha1
  foo calcite ankitsinghal
  foo calcite apache incubator-calcite
  foo calcite apilloud
  foo calcite arina-ielchiieva
  foo calcite arunmahadevan
  foo calcite aryeh-looker
  foo calcite asereda-gs
  foo calcite askarbozcan
  foo calcite asolimando
  foo calcite atris
  foo calcite axeisghost
  foo calcite b-slim
  foo calcite batytskyy
  foo calcite bchapuis
  foo calcite bchong95
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
  foo calcite chucheng92
  foo calcite chunhui-shi incubator-calcite
  foo calcite chunweilei
  foo calcite CodingCaproni
  foo calcite Contiamo
  foo calcite d4nc00per incubator-calcite
  foo calcite danny0405
  foo calcite darionyaphet
  foo calcite dasch-1
  foo calcite datametica
  foo calcite devozerov
  foo calcite devth incubator-calcite
  foo calcite dgloeckner
  foo calcite diandu remove
  foo calcite dianfu
  foo calcite dingguitao incubator-calcite
  foo calcite dkadams
  foo calcite docete
  foo calcite DonnyZone
  foo calcite dremio remove
  foo calcite dssysolyatin
  foo calcite dugenkui03
  foo calcite ebastien
  foo calcite elilevine
  foo calcite Enzo-Liu
  foo calcite eolivelli
  foo calcite ex00
  foo calcite F21
  foo calcite fib-seq
  foo calcite Fokko
  foo calcite Functor10
  foo calcite gabrielreid
  foo calcite gaodayue
  foo calcite GavinRay97
  foo calcite georgewfraser
  foo calcite gianm
  foo calcite Gkairi
  foo calcite godfreyhe
  foo calcite gooddata
  foo calcite gparai
  foo calcite hannerwang
  foo calcite haohui
  foo calcite hbtoo
  foo calcite HeartSaVioR
  foo calcite hequn8128
  foo calcite herunkang2018
  foo calcite hortonworks
  foo calcite hqx871
  foo calcite hsuanyi incubator-calcite
  foo calcite hsyuan
  foo calcite hustfxj
  foo calcite hzyuemeng1
  foo calcite ILuffZhe
  foo calcite itiels
  foo calcite jackscott6174
  foo calcite jacques-n incubator-calcite
  foo calcite jaltekruse
  foo calcite jamesstarr
  foo calcite JasonMing incubator-calcite
  foo calcite jaystarshot
  foo calcite jbalint
  foo calcite jcamachor
  foo calcite jduo
  foo calcite jeremiahrhall
  foo calcite jh3507
  foo calcite jhugomoore calcite-jhugomoore
  foo calcite JiajunBernoulli
  foo calcite jiayuanv127
  foo calcite jinfengni incubator-optiq
  foo calcite jinxing64
  foo calcite jnturton
  foo calcite joshelser incubator-calcite
  foo calcite jpullok remove
  foo calcite jpullokkaran
  foo calcite jswett77
  foo calcite jtrada168
  foo calcite JulianFeinauer
  foo calcite julianhyde
  foo calcite jxiang
  foo calcite kaiwangchen
  foo calcite KarshitShah
  foo calcite kasakrisz
  foo calcite kennknowles
  foo calcite kgyrtkirk
  foo calcite khaitranq calcite-1
  foo calcite kliewkliew
  foo calcite kstirman
  foo calcite KulykRoman incubator-calcite
  foo calcite KurtYoung
  foo calcite Kyligence
  foo calcite LakeShen
  foo calcite lalinsky incubator-calcite
  foo calcite lameyer
  foo calcite LantaoJin
  foo calcite laurentgo
  foo calcite leesf
  foo calcite LeoWangLZ
  foo calcite Lerm
  foo calcite lfkpoa
  foo calcite libenchao
  foo calcite lincoln-lil
  foo calcite LiShuMing
  foo calcite liuyongvs
  foo calcite liyafan82
  foo calcite looker-open-source
  foo calcite macroguo-ghy
  foo calcite maheshk114
  foo calcite mapr incubator-calcite
  foo calcite marcobjorge
  foo calcite markap14
  foo calcite maryannxue
  foo calcite masayuki038
  foo calcite MasseGuillaume
  foo calcite MGelbana
  foo calcite michaelmior
  foo calcite mihaibudiu
  foo calcite mikehinchey
  foo calcite milinda
  foo calcite mindcrime
  foo calcite minji-kim
  foo calcite mkou
  foo calcite mprudhom remove
  foo calcite msydoron
  foo calcite navis incubator-calcite
  foo calcite ndimiduk incubator-calcite
  foo calcite neoremind
  foo calcite nishantmonu51
  foo calcite NobiGo
  foo calcite olivrlee
  foo calcite pawelruchaj
  foo calcite pengchengxiong
  foo calcite pengzhiwei2018
  foo calcite ptrbojko
  foo calcite raghavSharmaCode
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
  foo calcite sambekar15
  foo calcite samwagg
  foo calcite sap-contributions
  foo calcite sbcd90
  foo calcite sergeysimonov
  foo calcite Serhii-Harnyk remove
  foo calcite shenh062326
  foo calcite shmushkis
  foo calcite siddharthteotia
  foo calcite sintown
  foo calcite smola incubator-calcite
  foo calcite snallapa
  foo calcite snuyanzin
  foo calcite sreev remove
  foo calcite stalbot
  foo calcite strongduanmu
  foo calcite stutigupta028
  foo calcite sudheeshkatkam incubator-calcite
  foo calcite suez1224
  foo calcite summerleafs
  foo calcite sunjincheng121
  foo calcite tanclary
  foo calcite tedxu
  foo calcite thomasbanghart
  foo calcite thomasrebele
  foo calcite tjbanghart
  foo calcite tmostak
  foo calcite traveler-guoke
  foo calcite twalthr incubator-calcite
  foo calcite tzolov
  foo calcite vaibhavjain-dm
  foo calcite VcamX
  foo calcite vdiravka
  foo calcite vijayk
  foo calcite viliam-durina
  foo calcite vineetgarg02
  foo calcite vkorukanti
  foo calcite vladimirdolzhenko
  foo calcite vladimirtkach
  foo calcite vlsi incubator-calcite
  foo calcite vrajat incubator-calcite
  foo calcite vvysotskyi
  foo calcite walterddr
  foo calcite wenhuitang
  foo calcite wenruimeng
  foo calcite wgorman optiq
  foo calcite wnob
  foo calcite wojustme
  foo calcite wuchong
  foo calcite wxiang7
  foo calcite xhoong incubator-calcite
  foo calcite Xpray
  foo calcite XuQianJin-Stars
  foo calcite yanghua
  foo calcite yanlin-Lynn
  foo calcite yeongwei incubator-calcite
  foo calcite yiming187
  foo calcite yingyuwang
  foo calcite yixinglu remove
  foo calcite yssharma incubator-optiq
  foo calcite yuqi1129
  foo calcite zabetak
  foo calcite zhifac
  foo calcite Zhiqiang-He
  foo calcite zhong-j-yu
  foo calcite zhztheplayer
  foo calcite zinking
  foo calcite zoudan
  foo calcite zstan
  ;;

(avatica|calcite-avatica)
  foo calcite-avatica amannm
  foo calcite-avatica asolimando
  foo calcite-avatica bcogrel
  foo calcite-avatica chenyuzhi459
  foo calcite-avatica devozerov
  foo calcite-avatica docete
  foo calcite-avatica F21
  foo calcite-avatica freastro
  foo calcite-avatica haohui
  foo calcite-avatica jackscott6174
  foo calcite-avatica JiajunBernoulli
  foo calcite-avatica joshelser
  foo calcite-avatica julianhyde
  foo calcite-avatica lalinsky
  foo calcite-avatica laurentgo
  foo calcite-avatica michaelmior
  foo calcite-avatica NobiGo
  foo calcite-avatica risdenk
  foo calcite-avatica snuyanzin
  foo calcite-avatica ssainz
  foo calcite-avatica zabetak
  foo calcite-avatica zstan
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

(morel)
  foo morel GavinRay97
  foo morel julianhyde
  ;;

(quidem)
  foo quidem NobiGo
  ;;

(share)
  foo share snuyanzin
  ;;

(sql-logic-test)
  foo sql-logic-test julianhyde
  foo sql-logic-test mihaibudiu
  foo sql-logic-test zabetak
  ;;

(sqlline)
  foo sqlline arina-ielchiieva
  foo sqlline bpoweski
  foo sqlline fineo-io
  foo sqlline IceMimosa
  foo sqlline jpesout
  foo sqlline kminder
  foo sqlline kmtong
  foo sqlline laurentedel
  foo sqlline liancheng
  foo sqlline madeye-matt
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
