#!/bin/bash
#
# Check out various apache svn folders I need for the projects I am
# interested in.

function svn-co() {
  (
    mkdir -p $(dirname $2)
    svn co $1 $2 &&
    cd $2 &&
    # svn upgrade &&
    svn cleanup &&
    svn update
  ) 2>&1 | sed -e "s!^!$2: !"
}

svn-co https://svn.apache.org/repos/private/foundation ~/apache/private/foundation &
svn-co https://svn.apache.org/repos/private/committers/board ~/apache/private/committers/board &
svn-co https://svn.apache.org/repos/private/committers/info ~/apache/private/committers/info &

svn-co https://svn.apache.org/repos/asf/comdev/projects.apache.org ~/apache/asf/comdev/projects.apache.org &
svn co https://svn.apache.org/repos/asf/incubator/public/trunk ~/apache/asf/incubator &
svn co https://svn.apache.org/repos/asf/incubator/donations ~/apache/asf/incubator/donations &
svn co https://svn.apache.org/repos/asf/infrastructure/site/trunk ~/apache/asf/infrastructure/site &

for i in kylin \
    incubator/crail \
    incubator/quickstep \
    arrow \
    calcite \
    metron
do
  svn-co https://dist.apache.org/repos/dist/dev/$i ~/apache/dist/dev/$i &
  svn-co https://dist.apache.org/repos/dist/release/$i ~/apache/dist/release/$i &
done

wait

# End apache-svn.sh
