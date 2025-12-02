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

func sumInvalidIdsInRange(lower int, upper int) int {
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

func part1(filename string) {
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
		lower, _ := strconv.Atoi(splitRange[0])
		upper, _ := strconv.Atoi(splitRange[1])

		totalInvalidIds += sumInvalidIdsInRange(lower, upper)
	}

	fmt.Println(totalInvalidIds)
}

func main() {
	part1(os.Args[1])
}
