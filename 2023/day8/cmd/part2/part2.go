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

func ParseInstructions(scanner *bufio.Scanner) ([]string, []Neighbours, []int) {
    labels := make([]string, 0)
    stringNeighbours := make([][]string, 0)
    inverseMapping := map[string]int{}

    labelID := 0
    starts := make([]int, 0)
    for scanner.Scan() {
        line := scanner.Text()
        if line == "" {
            continue
        }
        label, left, right := parseInstructionLine(line)
        stringNeighbours = append(stringNeighbours, []string{left, right})
        labels = append(labels, label)
        inverseMapping[label] = labelID
        if label[2] == 'A' {
            starts = append(starts, labelID)
        }
        labelID++
    }

    neighbours := make([]Neighbours, labelID)
    for i, x := range stringNeighbours {
        neighbours[i] = Neighbours{left:inverseMapping[x[0]], right:inverseMapping[x[1]]}
    }
    return labels, neighbours, starts
}

// from https://siongui.github.io/2017/06/03/go-find-lcm-by-gcd/
func gcd(a, b int) int {
      for b != 0 {
              t := b
              b = a % b
              a = t
      }
      return a
}
func lcm(a, b int, integers ...int) int {
      result := a * b / gcd(a, b)

      for i := 0; i < len(integers); i++ {
              result = lcm(result, integers[i])
      }

      return result
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
    labels, neighbours, starts := ParseInstructions(scanner)

    n := len(directions)
    steps := make([]int, 0)
    for _, pos := range starts {
        step := 0
        for labels[pos][2] != 'Z' {
            if directions[step % n] {
                pos = neighbours[pos].left
            } else {
                pos = neighbours[pos].right
            }
            step++
        }
        steps = append(steps, step)
    }

    res := 0
    switch len(steps) {
    case 1:
        res = steps[0]
    case 2:
        res = lcm(steps[0], steps[1])
    default:
        res = lcm(steps[0], steps[1], steps[2:]...)
    }
    fmt.Printf("Steps required: %d\n", res)
}
