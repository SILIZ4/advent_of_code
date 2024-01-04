package util

import "errors"

const (
    Up = iota
    Down = iota
    Left = iota
    Right = iota
)

type Pipe [4]bool

func ParseLine(line string) ([]Pipe, int) {
    pipes := make([]Pipe, 0, 16)
    startCol := -1
    for i, c := range line {
        var up, down, left, right bool // default is false
        switch c {
        case '|':
            up = true
            down = true
        case '-':
            left = true
            right = true
        case 'L':
            up = true
            right = true
        case 'J':
            up = true
            left = true
        case '7':
            left = true
            down = true
        case 'F':
            down = true
            right = true
        case 'S':
            startCol = i
        }
        pipes = append(pipes, Pipe{up, down, left, right})
    }
    return pipes, startCol
}

func NextDirection(from int, pipe Pipe) (int, error) {
    for i, connected := range pipe {
        if connected && i != from {
            return i, nil
        }
    }
    return 0, errors.New("Unable to find next pipe.")
}
