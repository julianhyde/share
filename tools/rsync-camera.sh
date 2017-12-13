#!/bin/bash

rsync -arP /Volumes/Untitled/DCIM marmite:web/pix/staging/j6d
ssh marmite "fiximg.sh --prefix j6d /home/jhyde/web/pix/staging/j6d"

# End

