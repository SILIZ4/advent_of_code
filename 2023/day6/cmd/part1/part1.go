package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
    "day6/internal/util"
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
    raceDuration := util.ParseLine(re.Split(scanner.Text(), -1))
    scanner.Scan()
    raceRecord := util.ParseLine(re.Split(scanner.Text(), -1))

    var prod int64 = 1
    for i := range raceDuration {
        holdMin, holdMax, err := util.FindWinningInterval(raceDuration[i], raceRecord[i])
        if err == nil {
            prod *= holdMax-holdMin+1
        } else {
            prod = 0
        }
    }
    fmt.Printf("Possible ways to win: %d.\n", prod)
}
