#!/bin/bash
# Create all the git remotes
#
# Create as follows:
#   git remote -v |awk '{printf "git remote add %s %s\n", $1, $2}' | sort -u
case "$1" in
(calcite)
  git remote add gparai https://github.com/gparai/calcite.git
  git remote add aertoria https://github.com/aertoria/incubator-calcite.git
  git remote add aleph-zero git@github.com:aleph-zero/incubator-calcite.git
  git remote add amansinha100 git@github.com:amansinha100/incubator-calcite.git
  git remote add amoghmargoor git@github.com:amoghmargoor/incubator-calcite.git
  git remote add apache git@github.com:apache/incubator-calcite.git
  git remote add arina-ielchiieva git@github.com:arina-ielchiieva/calcite.git
  git remote add bdumon git@github.com:bdumon/optiq.git
  git remote add benoyantony git@github.com:benoyantony/incubator-optiq.git
  git remote add d4nc00per git@github.com:d4nc00per/incubator-calcite.git
  git remote add devth git@github.com:devth/incubator-calcite.git
  git remote add dingguitao git@github.com:dingguitao/incubator-calcite.git
  git remote add dremio https://github.com/dremio/calcite.git
  git remote add ebastien https://github.com/ebastien/calcite.git
  git remote add gabrielreid git@github.com:gabrielreid/optiq.git
  git remote add gparai https://github.com/gparai/calcite.git
  git remote add hortonworks git@github.com:hortonworks/calcite.git
  git remote add hsuanyi git@github.com:hsuanyi/incubator-calcite.git
  git remote add jacques-n git@github.com:jacques-n/incubator-calcite.git
  git remote add jaltekruse git@github.com:jaltekruse/calcite.git
  git remote add JasonMing git@github.com:JasonMing/incubator-calcite.git
  git remote add jcamachor git@github.com:jcamachor/calcite.git
  git remote add jinfengni git@github.com:jinfengni/incubator-optiq.git
  git remote add joshelser git@github.com:joshelser/incubator-calcite.git
  git remote add jpullokk https://github.com/jpullokkaran/calcite.git
  git remote add julianhyde git@github.com:julianhyde/calcite.git
  git remote add jxiang https://github.com/jxiang/calcite.git
  git remote add lalinsky git@github.com:lalinsky/incubator-calcite.git
  git remote add mapr git@github.com:mapr/incubator-calcite.git
  git remote add maryannxue git@github.com:maryannxue/calcite.git
  git remote add michaelmior git@github.com:michaelmior/calcite.git
  git remote add mikehinchey https://github.com/mikehinchey/calcite.git
  git remote add milinda https://github.com/milinda/calcite.git
  git remote add mindcrime https://github.com/mindcrime/calcite.git
  git remote add minji-kim git@github.com:minji-kim/calcite.git
  git remote add mprudhom https://github.com/mprudhom/incubator-optiq.git
  git remote add navis git@github.com:navis/incubator-calcite.git
  git remote add ndimiduk git@github.com:ndimiduk/incubator-calcite.git
  git remote add origin https://git-wip-us.apache.org/repos/asf/calcite.git
  git remote add rmetzger git@github.com:rmetzger/optiq.git
  git remote add sbcd90 https://github.com/sbcd90/calcite.git
  git remote add smola https://github.com/smola/incubator-calcite.git
  git remote add sreev git@github.com:sreev/incubator-optiq.git
  git remote add sudheeshkatkam git@github.com:sudheeshkatkam/incubator-calcite.git
  git remote add tedxu git@github.com:tedxu/calcite.git
  git remote add vkorukanti git@github.com:vkorukanti/calcite.git
  git remote add vlsi git@github.com:vlsi/incubator-calcite.git
  git remote add vrajat git@github.com:vrajat/incubator-calcite.git
  git remote add wgorman git@github.com:wgorman/optiq.git
  git remote add xhoong git@github.com:xhoong/incubator-calcite.git
  git remote add yeongwei git@github.com:yeongwei/incubator-calcite.git
  git remote add yiming187 https://github.com/yiming187/calcite.git
  git remote add yixinglu https://github.com/yixinglu/calcite.git
  git remote add yssharma git@github.com:yssharma/incubator-optiq.git
  git remote add zhifac git@github.com:zhifac/calcite.git
  git remote add zinking git@github.com:zinking/calcite.git
  ;;
esac

# End add-remotes.sh
