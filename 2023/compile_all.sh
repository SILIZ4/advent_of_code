#!/usr/bin/env bash

for day in {1..25}; do
    echo -ne "Compiling ... ${day}/25\r"
    if [[ ${day} -lt 6 ]]; then
        cd day${day}; make > /dev/null; cd ..
    elif [[ ${day} -lt 11 ]]; then
        cd day${day}; go build cmd/part1/part1.go; go build cmd/part2/part2.go; cd ..
    elif [[ ${day} -lt 16 ]]; then
        cd day${day}; mix compile > /dev/null; cd ..
    fi
done
echo -e "\nDone"
