package util

import (
    "strconv"
    "math"
    "errors"
)

func ParseLine(line []string) []int64 {
    var parsedString []int64
    for _, el := range line[1:] {
        value, err := strconv.ParseInt(el, 10, 64)
        if err != nil {
            panic("Incorrect file formatting")
        }
        parsedString = append(parsedString, value)
    }
    return parsedString
}

func isIntegral(x float64) bool {
    return x == float64(int(x))
}

func FindWinningInterval(duration, record int64) (int64, int64, error) {
    floatDuration := float64(duration)
    discriminant := math.Sqrt(floatDuration*floatDuration - 4.*float64(record))
    if math.IsNaN(discriminant) {
        return 0, 0, errors.New("Race is unwinnable")
    }

    var holdMin, holdMax int64
    if t := 0.5*(floatDuration - discriminant); isIntegral(t) {
        holdMin = int64(t)+1
    } else {
        holdMin = int64(math.Ceil(t))
    }
    if t := 0.5*(floatDuration + discriminant); isIntegral(t) {
        holdMax = int64(t)-1
    } else {
        holdMax = int64(math.Floor(t))
    }
    if holdMin > holdMax {
        return 0, 0, errors.New("Race is unwinnable")
    }
    return holdMin, holdMax, nil
}
