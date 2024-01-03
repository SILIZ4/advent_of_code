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
    firstValues := []int{}
    copy(previousSeq, seq)
    noDiff := false
    for !noDiff || len(previousSeq) == 0 {
        firstValues = append(firstValues, previousSeq[0])
        noDiff = true
        for i:=len(previousSeq)-2; i>=0; i-- {
            diff := previousSeq[i+1] - previousSeq[i]
            if diff!=0 {
                noDiff = false
            }
            previousSeq[i+1] = diff
        }
        previousSeq = previousSeq[1:]
    }
    prediction := 0
    for i:=len(firstValues)-1; i>=0; i-- {
        prediction = firstValues[i] - prediction
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
