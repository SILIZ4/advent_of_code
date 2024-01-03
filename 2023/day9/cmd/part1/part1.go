package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func obtainInt(digits []rune) int {
    v, err := strconv.Atoi(string(digits))
    if err != nil {
        panic("Invalid line in file.")
    }
    return v
}

func parseLine(line string) []int {
    convertedSeq := []int{}
    digits := []rune{}
    for _, c := range line {
        if c == ' ' {
            convertedSeq = append(convertedSeq, obtainInt(digits))
            digits = []rune{}
        } else {
            digits = append(digits, c)
        }
    }
    convertedSeq = append(convertedSeq, obtainInt(digits))
    return convertedSeq
}

func predict(seq []int) int {
    previousSeq := make([]int, len(seq))
    copy(previousSeq, seq)
    noDiff := false
    prediction := 0
    for !noDiff || len(previousSeq) == 0 {
        prediction += previousSeq[len(previousSeq)-1]
        noDiff = true
        for i:=1; i<len(previousSeq); i++ {
            diff := previousSeq[i] - previousSeq[i-1]
            if diff!=0 {
                noDiff = false
            }
            previousSeq[i-1] = diff
        }
        previousSeq = previousSeq[:len(previousSeq)-1]
    }
    return prediction
}


func main() {
    args := os.Args
    if len(args) != 2 {
        panic("Incorrect args")
    }
    file, err := os.Open(args[1])
    if err != nil {
        panic("Couldn't open file")
    }

    scanner := bufio.NewScanner(file)
    sum := 0
    for scanner.Scan() {
        seq := parseLine(scanner.Text())
        prediction := predict(seq)
        sum += prediction
    }
    fmt.Printf("Sum is: %d\n", sum)
}
