package main

import (
	"bufio"
	"day10/internal/util"
	"os"
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

    scanner := bufio.NewScanner(file)
    pipeMap := make([][]util.Pipe, 0, 16)
    row := 0
    startCol := -1
    startRow := -1
    for scanner.Scan() {
        pipes, col := util.ParseLine(scanner.Text())
        pipeMap = append(pipeMap, pipes)
        if col != -1 {
            startCol = col
            startRow = row
        }
        row++
    }

    if startCol == -1 || startRow == -1 {
        panic("No starting point found.")
    }

    pos := [2]int{}
    var from int
    // starting arbitrarily from first connected pipe found
    if row:= startRow-1; startRow > 0 && pipeMap[row][startCol][util.Down] {
        pos = [2]int{row, startCol}
        from = util.Down
    } else if row := startRow+1; startRow < len(pipeMap)-1 && pipeMap[row][startCol][util.Up] {
        pos = [2]int{row, startCol}
        from = util.Up
    } else if col := startCol-1; startCol > 0 && pipeMap[startRow][col][util.Right] {
        pos = [2]int{startRow, col}
        from = util.Right
    } else if col := startCol+1; startCol < len(pipeMap[0])-1 && pipeMap[startRow][col][util.Left] {
        from = util.Left
        pos = [2]int{startRow, col}
    } else {
        panic("No pipe connected to start.")
    }

    pipeInCycle := make([][]bool, 0, len(pipeMap))
    cols := len(pipeMap[0])
    for range pipeMap {
        pipeInCycle = append(pipeInCycle, make([]bool, cols))
    }

    startingPos := [2]int{startRow, startCol}
    cycleLength := 0
    cycleCoordinates := [][2]int{pos}
    for pos != startingPos {
        next, err := util.NextDirection(from, pipeMap[pos[0]][pos[1]])
        if err != nil {
            panic(err.Error())
        }
        switch next {
        case util.Up:
            pos[0]--
            from = util.Down
        case util.Down:
            pos[0]++
            from = util.Up
        case util.Left:
            pos[1]--
            from = util.Right
        case util.Right:
            pos[1]++
            from = util.Left
        }
        cycleLength++;
        cycleCoordinates = append(cycleCoordinates, pos)
    }

    // Shoelace formula
    totalArea := 0
    for i:=0; i<cycleLength; i++ {
        var x_1, y_1 int
        if i==0 {
            x_1 = cycleCoordinates[cycleLength-1][0]
            y_1 = cycleCoordinates[cycleLength-1][1]
        } else {
            y_1 = cycleCoordinates[i-1][1]
            x_1 = cycleCoordinates[i-1][0]
        }
        totalArea += cycleCoordinates[i][0]*y_1 - x_1*cycleCoordinates[i][1]
    }
    if totalArea < 0 {
        totalArea = -totalArea
    }
    // Pick's theorem
    println("Surrounded area is", totalArea/2-(cycleLength+cycleLength%2)/2+1)
}
