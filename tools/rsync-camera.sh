#!/bin/bash

case $(uname -s) in
(Linux) BASE='/media/jhyde/9C33-6BBD'; DEST=${HOME}/;;
(*) BASE='/Volumes/Untitled'; DEST='marmite:';;
esac

rsync -arP --filter='- ._*.JPG' ${BASE}/DCIM ${DEST}web/pix/staging/j6d

case $(uname -n) in
(marmite) fiximg.sh --prefix j6d /home/jhyde/web/pix/staging/j6d;;
(*) ssh marmite "fiximg.sh --prefix j6d /home/jhyde/web/pix/staging/j6d";;
esac

# End

