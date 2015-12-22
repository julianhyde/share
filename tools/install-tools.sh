#!/bin/bash
# Install tools in /usr/local/bin

cd $(dirname $0)
if [ $(pwd) = /usr/local/bin ]; then
  exit
fi
for i in \
  apache-svn.sh \
  calcite-regress.sh \
  checkKeys.sh \
  emacsValidate \
  extra.awk \
  extra.sh \
  gra \
  relNotes \
  remove-javadoc-timestamps \
  mailself
do
  ln -s $(pwd)/$i /usr/local/bin
done

# End install-tools.sh
