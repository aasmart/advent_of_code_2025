package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
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

	fmt.Println(maxSoFar)
	return maxSoFar
}

func parseInput(filename string, process func(string) int) {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}

	total_joltage := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		bank := scanner.Text()
		fmt.Println(bank)
		total_joltage += process(bank)
	}

	fmt.Println(total_joltage)
}

func main() {
	parseInput(os.Args[1], maxJoltage)
}
