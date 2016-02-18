#!/bin/bash
#
# Check out various apache svn folders I need for the projects I am
# interested in.

function svn-co() {
    svn co $1 $2
    (cd $2; svn up)
}

mkdir -p ~/apache/private/foundation

svn-co https://svn.apache.org/repos/private/foundation/board ~/apache/private/foundation/board
svn-co https://svn.apache.org/repos/private/foundation/officers ~/apache/private/foundation/officers
svn-co https://svn.apache.org/repos/private/committers/board ~/apache/private/committers/board
svn-co https://svn.apache.org/repos/private/committers/info ~/apache/private/committers/info

svn co https://svn.apache.org/repos/asf/incubator/public/trunk ~/apache/asf/incubator
svn co https://svn.apache.org/repos/asf/infrastructure/site/trunk ~/apache/asf/infrastructure/site

for i in kylin incubator/concerted incubator/eagle calcite # drill
do
  svn-co https://dist.apache.org/repos/dist/dev/$i ~/apache/dist/dev/$i
  svn-co https://dist.apache.org/repos/dist/release/$i ~/apache/dist/release/$i
done

# End apache-svn.sh
