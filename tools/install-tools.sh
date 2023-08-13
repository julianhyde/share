#!/bin/bash
# Install tools in /usr/local/bin
#
# The following scripts are in tools but we don't need them on the path:
# barclay, relMail.sh
#
# We create a link for 'jenv' in /usr/local but it is not executable. This
# reminds to source it (using '.') not run it.

cd $(dirname $0)
if [ $(pwd) = /usr/local/bin ]; then
  exit
fi
for i in \
  add-remotes.sh \
  apache-svn.sh \
  avi2mp4 \
  backup \
  calcite-regress.sh \
  calcite-regress2.sh \
  checkKeys.sh \
  e \
  ediff \
  extra.awk \
  extra.sh \
  fiximg.sh \
  fullMake \
  globa \
  gpg-export \
  gra \
  jenv \
  lastLog.sh \
  relNotes \
  remove-javadoc-timestamps \
  mailself \
  rat \
  rsync-camera.sh \
  te \
  update-dns.sh \
  validate
do
  ln -s $(pwd)/$i /usr/local/bin
done

ln -s $(dirname $(pwd))/lisp ~
ln -s $(dirname $(pwd))/lisp/_emacs ~/.emacs

# End install-tools.sh
