#!/usr/bin/env bash

for day in {1..25}; do
    daydir="./day${day}"
    if $(test -f ${daydir}/part1) && $(test -f ${daydir}/part2); then
        echo "Day ${day}: "
        hyperfine --runs 10 --shell=none "${daydir}/part1 day${day}/data.txt"
        hyperfine --runs 10 --shell=none "${daydir}/part2 day${day}/data.txt"
    else
        echo "Day ${day}: -"
    fi

done
echo -e "\nDone"
