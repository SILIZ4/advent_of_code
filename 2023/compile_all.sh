#!/usr/bin/env bash

for day in {1..25}; do
    echo -ne "Compiling ... ${day}/25\r"
    dir=day${day}
    if [ ! -d "${dir}" ]; then
        continue
    fi
    cd ${dir}
    if [[ ${day} -lt 6 ]]; then
        make > /dev/null
    elif [[ ${day} -lt 11 ]]; then
        go build cmd/part1/part1.go; go build cmd/part2/part2.go
    elif [[ ${day} -lt 16 ]]; then
        mix compile > /dev/null
    elif [[ ${day} -lt 21 ]]; then
        cabal build -O2 > /dev/null && cabal install --installdir=. > /dev/null
    fi
    cd ..
done
