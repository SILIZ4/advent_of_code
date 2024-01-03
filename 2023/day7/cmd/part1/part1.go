package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
    "day7/internal/util"
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

    results := make(map[string][]int)
    for _, t := range util.HandTypes {
        results[t] = []int{}
    }

    hands := [][]rune{}
    bids := []int{}
    handID:=0
    for scanner.Scan() {
        text := []rune(scanner.Text())
        handType := util.GetHandType(text[:5])
        results[handType] = append(results[handType], handID)
        hands = append(hands, text[:5])
        bid, err := strconv.Atoi(string(text[6:]))
        if err != nil {
            panic("Invalid file format.")
        }
        bids = append(bids, bid)
        handID++
    }

    evaluateCard := func (card rune) int {
        return util.CardValue(card, []rune("23456789TJQKA"))
    }
    for _, equalHands := range results {
        if len(equalHands) > 1 {
            sort.Slice(equalHands, func(i, j int) bool {
                return util.WorseHand(hands[equalHands[i]], hands[equalHands[j]], evaluateCard)
            })
        }
    }

    rank := 1
    sum := 0

    for _, handType := range util.HandTypes {
        for _, hand := range results[handType] {
            sum += rank*bids[hand]
            rank++
        }
    }
    fmt.Printf("Sum is %d\n", sum)
}
