package main

import (
	"bufio"
	"fmt"
	"os"
)

func ParseDirections(line string) []bool {
    turnLeft := make([]bool, len(line))
    for i, c := range line {
        turnLeft[i] = c == 'L'
    }
    return turnLeft
}

type Neighbours struct {
    left int
    right int
}

func parseInstructionLine(line string) (string, string, string) {
    var label, left, right string
    _, err := fmt.Sscanf(line, "%s = (%3s, %3s)", &label, &left, &right)
    if err != nil {
        panic("Couldn't parse intruction line.")
    }
    return label, left, right
}

func ParseInstructions(scanner *bufio.Scanner) ([]string, []Neighbours, int, int) {
    labels := make([]string, 0)
    stringNeighbours := make([][]string, 0)
    inverseMapping := map[string]int{}

    labelID := 0
    end := 0
    start := 0
    for scanner.Scan() {
        line := scanner.Text()
        if line == "" {
            continue
        }
        label, left, right := parseInstructionLine(line)
        stringNeighbours = append(stringNeighbours, []string{left, right})
        labels = append(labels, label)
        inverseMapping[label] = labelID
        if label == "ZZZ" {
            end = labelID
        } else if label == "AAA" {
            start = labelID
        }
        labelID++
    }

    neighbours := make([]Neighbours, labelID)
    for i, x := range stringNeighbours {
        neighbours[i] = Neighbours{left:inverseMapping[x[0]], right:inverseMapping[x[1]]}
    }
    return labels, neighbours, start, end
}


func main() {
    args := os.Args
    if len(args) != 2 {
        panic("Incorrect args.")
    }

    file, err := os.Open(args[1])
    if err != nil {
        panic("Couldn't open file.")
    }

    scanner := bufio.NewScanner(file)
    scanner.Scan()
    directions := ParseDirections(scanner.Text())
    _, neighbours, pos, end := ParseInstructions(scanner)

    var step int64 = 0
    n := int64(len(directions))

    for pos != end {
        if directions[step % n] {
            pos = neighbours[pos].left
        } else {
            pos = neighbours[pos].right
        }
        step++
    }

    fmt.Printf("Steps required: %d\n", step)
}
