package util

import "strconv"


var HandTypes = []string{"HC", "1", "P", "2P", "3", "FH", "4", "5"}

func CountOccurences(chars []rune) map[rune]int {
    occurences := make(map[rune]int)
    for _, c := range chars {
        _, ok := occurences[c]
        if !ok {
            occurences[c] = 1
        } else {
            occurences[c]++
        }
    }
    return occurences
}

func CardValue(card rune, cardRanking[]rune) int {
    for value, c := range cardRanking {
        if c == card {
            return value
        }
    }
    return 0
}

func WorseHand(hand1, hand2 []rune, cardValue func (rune) int) bool {
    for j := 0; j<5; j++ {
        val1 := cardValue(hand1[j])
        val2 := cardValue(hand2[j])
        if val1 != val2 {
            return val1 < val2
        }
    }
    return false
}

func first(m map[rune]int) (rune, int) {
    for key, val := range m {
        return key, val
    }
    return 0, 0
}

func GetJokerHandType(hand []rune) string {
    cardOccs := CountOccurences(hand)
    jokerCount, ok := cardOccs['J']
    if !ok {
        return getHandType(hand, cardOccs)
    }
    delete(cardOccs, 'J')
    if len(cardOccs)+jokerCount == 5 {
        highestCount := 0
        for _, count := range cardOccs {
            if count > highestCount {
                highestCount = count
            }
        }
        res := strconv.Itoa(highestCount+jokerCount)
        if res == "2" {
            return "P"
        }
        return res
    }
    switch len(cardOccs) {
    case 3:
        return "3"
    case 2:
        if jokerCount==2 {
            return "4"
        }
        if _, val := first(cardOccs); val == 2 {
            return "FH"
        }
        return "4"
    case 1:
        return "5"
    }
    return ""
}


func GetHandType(hand []rune) string {
    return getHandType(hand, CountOccurences(hand))
}

func getHandType(hand []rune, cardOccs map[rune]int) string {
    switch len(cardOccs) {
    case 5 :{
        return "HC"
    }
    case 4 :{
        return "P"
    }
    case 3: {
        highestCount := 0
        for _, count := range cardOccs {
            if count > highestCount {
                highestCount = count
            }
        }
        if highestCount == 3 {
            return "3"
        } else {
            return "2P"
        }
    }
    case 2: {
        firstCardOcc := cardOccs[hand[0]]
        if firstCardOcc == 2 || firstCardOcc == 3 {
            return "FH"
        } else {
            return "4"
        }
    }
    case 1: {
        return "5"
    }
    }
    return ""
}
