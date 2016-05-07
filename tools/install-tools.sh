#!/bin/bash
# Install tools in /usr/local/bin
#
# The following scripts are in tools but we don't need them on the path:
# barclay, relMail.sh

cd $(dirname $0)
if [ $(pwd) = /usr/local/bin ]; then
  exit
fi
for i in \
  apache-svn.sh \
  avi2mp4 \
  calcite-regress.sh \
  calcite-regress2.sh \
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

ln -s $(dirname $(pwd))/lisp ~
ln -s $(dirname $(pwd))/lisp/_emacs ~/.emacs

# End install-tools.sh
