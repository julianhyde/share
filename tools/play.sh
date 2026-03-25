#!/bin/bash
cd $(dirname $0)/../scratch
if [ ! -f target/classes/net/hydromatic/scratch/Play.class ]; then
    ./mvnw -DskipTests clean compile
fi
exec java -cp target/classes net.hydromatic.scratch.Play "$@"
