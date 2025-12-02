package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func numDigitsInNum(num int) int {
	if num == 0 {
		return 1
	}

	digits := 0
	for num != 0 {
		digits += 1
		num /= 10
	}

	return digits
}

func sumInvalidIdsInRange(lowerStr string, upperStr string) int {
	lower, _ := strconv.Atoi(lowerStr)
	upper, _ := strconv.Atoi(upperStr)

	totalInvalidNums := 0

	numDigitsInLower := numDigitsInNum(lower)
	if numDigitsInLower%2 != 0 {
		numDigitsInLower -= 1
	}

	currSplitNumber := lower / int(math.Pow10((numDigitsInLower / 2)))
	for {
		invalidID := currSplitNumber*int(math.Pow10(numDigitsInNum(currSplitNumber))) + currSplitNumber
		if invalidID > upper {
			break
		}

		currSplitNumber += 1

		// case where number begins less than lower
		if invalidID < lower {
			continue
		}

		totalInvalidNums += invalidID
	}

	return totalInvalidNums
}

func sumInvalidIsInRangePart2(lower string, upper string) int {
	totalInvalidNums := 0

	lowerNum, _ := strconv.Atoi(lower)
	upperNum, _ := strconv.Atoi(upper)
	currNum := lowerNum
	for currNum <= upperNum {
		currNumStr := strconv.Itoa(currNum)
		currNumLength := len(currNumStr)

		for i := range currNumLength / 2 {
			repeatStr := currNumStr[:i+1]
			if currNumLength%(i+1) != 0 {
				continue
			}

			num, _ := strconv.Atoi(strings.Repeat(repeatStr, currNumLength/(i+1)))

			if num == currNum {
				totalInvalidNums += num
				break
			}
		}

		currNum += 1
	}

	return totalInvalidNums
}

func processInput(filename string, id_check func(string, string) int) {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}

	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	line := scanner.Text()

	totalInvalidIds := 0

	ranges := strings.SplitSeq(line, ",")
	for r := range ranges {
		splitRange := strings.Split(r, "-")
		totalInvalidIds += id_check(splitRange[0], splitRange[1])
	}

	fmt.Println(totalInvalidIds)
}

func main() {
	processInput(os.Args[1], sumInvalidIdsInRange)
	processInput(os.Args[1], sumInvalidIsInRangePart2)
}
