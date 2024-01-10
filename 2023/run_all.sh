#!/usr/bin/env bash

for day in {1..25}; do
    daydir="./day${day}"
    if $(test -f ${daydir}/part1) && $(test -f ${daydir}/part2); then
        echo "Day ${day}: "
        cd "${daydir}"
        hyperfine --runs 10 --shell=none "./part1 data.txt"
        hyperfine --runs 10 --shell=none "./part2 data.txt"
        cd ..
    else
        echo "Day ${day}: -"
    fi

done
echo -e "\nDone"
