#!/bin/bash
# Script to convert a slideshow into an animated GIF
convert ./calcite-zetasql-nwds-2021.pdf ./calcite-zetasql-nwds-2021-%03d.png
convert \
    -delay 80 \
    -loop 0 \
    ./calcite-zetasql-nwds-2021-0{13,13,13,13,19,02,02,02,03,04,04,04,05,05,05,06,06,06,07,07,07,08,08,08,09,09,09,10,10,10,10,11,11,11,12,12,12,19}.png \
    ./calcite-zetasql-nwds-2021.gif
ls -lh ./calcite-zetasql-nwds-2021.gif
