package main

import (
	"bufio"
	"day6/internal/util"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
    args := os.Args
    if len(args) != 2 {
        panic("Incorrect args.")
    }
    file, err := os.Open(args[1])
    if err != nil {
        panic("Couldn't open file.")
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    re := regexp.MustCompile(`\s+`)

    scanner.Scan()
    raceDuration, err := strconv.ParseInt(strings.Join(re.Split(scanner.Text(), -1)[1:], ""), 10, 64)
    if err != nil {
        panic("Incorrect file format.")
    }
    scanner.Scan()
    raceRecord, err := strconv.ParseInt(strings.Join(re.Split(scanner.Text(), -1)[1:], ""), 10, 64)
    if err != nil {
        panic("Incorrect file format.")
    }

    var prod int64 = 0
    holdMin, holdMax, err := util.FindWinningInterval(raceDuration, raceRecord)
    if err == nil {
        prod = holdMax-holdMin+1
    } else {
        prod = 0
    }
    fmt.Printf("Possible ways to win: %d.\n", prod)
}
