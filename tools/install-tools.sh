#!/bin/bash
# Install tools in /usr/local/bin

cd $(dirname $0)
if [ $(pwd) = /usr/local/bin ]; then
  exit
fi
for i in \
  emacsValidate \
  extra.awk \
  extra.sh
do
  ln -s $(pwd)/$i /usr/local/bin
done

# End install-tools.sh
