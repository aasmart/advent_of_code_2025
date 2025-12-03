package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func maxJoltage(bank string) int {
	maxSoFar := 0
	currMaxDigit := -1
	for _, charge := range bank {
		chargeNum := int(charge - '0')
		if currMaxDigit != -1 {
			maxSoFar = max(maxSoFar, currMaxDigit*10+chargeNum)
		}

		if chargeNum > currMaxDigit {
			currMaxDigit = chargeNum
		}
	}

	return maxSoFar
}

func maxOverloadJoltage(bank string) int {
	joltage := ""

	for i, charge := range bank {
		chargeNum := int(charge - '0')
		// the second check just ensures that we cna remove this number and have enough left to make 12 digits
		for len(joltage) > 0 && len(joltage)+len(bank)-i-1 >= 12 && chargeNum > int(joltage[len(joltage)-1]-'0') {
			joltage = joltage[:len(joltage)-1]
		}
		if len(joltage) < 12 {
			joltage += string(charge)
		}
	}

	val, _ := strconv.Atoi(joltage)
	return val
}

func parseInput(filename string, process func(string) int) {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}

	totalJoltage := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		bank := scanner.Text()
		totalJoltage += process(bank)
	}

	fmt.Println(totalJoltage)
}

func main() {
	parseInput(os.Args[1], maxJoltage)
	parseInput(os.Args[1], maxOverloadJoltage)
}
