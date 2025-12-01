package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func part1(filename string) {
	f, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	dialPos := 50
	zeroCount := 0

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		action := scanner.Text()
		direction := action[0]
		amount, err := strconv.Atoi(action[1:])
		if err != nil {
			log.Fatal(err)
		}

		if direction == 'L' {
			dialPos = (dialPos - amount) % 100
		} else if direction == 'R' {
			dialPos = (amount + dialPos) % 100
		}

		if dialPos == 0 {
			zeroCount += 1
		}
	}

	fmt.Println(zeroCount)
}

// dialPosA := 50
// zeroCountA := 0
//
//	for range amount {
//		if direction == 'L' {
//			dialPosA = (dialPosA - 1) % 100
//		} else if direction == 'R' {
//			dialPosA = (dialPosA + 1) % 100
//		}
//
//		if dialPosA == 0 {
//			zeroCountA += 1
//		}
//	}

func part2(filename string) {
	f, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	dialPos := 50
	zeroCount := 0

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		action := scanner.Text()
		direction := action[0]
		amount, err := strconv.Atoi(action[1:])
		if err != nil {
			log.Fatal(err)
		}

		beforePos := dialPos
		switch direction {
		case 'L':
			dialPos = (dialPos - (amount % 100)) % 100
			if dialPos < 0 {
				dialPos += 100
			}
		case 'R':
			dialPos = (amount + dialPos) % 100
		}

		// we need to catch the initial rotation through 0
		if beforePos != 0 && (dialPos == 0 || (dialPos >= beforePos && direction == 'L') || (dialPos <= beforePos && direction == 'R')) {
			zeroCount += 1
		}

		// add the remainder of the rotation
		zeroCount += amount / 100
	}

	fmt.Println(zeroCount)
}

func main() {
	filename := os.Args[1]
	part1(filename)
	part2(filename)
}
