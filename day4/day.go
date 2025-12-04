package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func part1(grid []string) int {
	totalAccessible := 0

	for row, str := range grid {
		for col, currTile := range str {
			if currTile != '@' {
				continue
			}

			adjacent := 0
			for dRow := -1; dRow <= 1; dRow += 1 {
				for dCol := -1; dCol <= 1; dCol += 1 {
					if dRow == 0 && dCol == 0 {
						continue
					}
					if row+dRow >= 0 && col+dCol >= 0 && row+dRow <= len(grid)-1 && col+dCol <= len(str)-1 && grid[row+dRow][col+dCol] == '@' {
						adjacent += 1
					}
				}
			}

			if adjacent < 4 {
				totalAccessible += 1
			}
		}
	}

	return totalAccessible
}

type pos struct {
	row int
	col int
}

func part2(grid []string) int {
	totalAccessible := 0

	posNumAdjacentRolls := make([][]int, len(grid))
	for i := range posNumAdjacentRolls {
		posNumAdjacentRolls[i] = make([]int, len(grid[i]))
	}

	currentAccessibleRolls := []pos{}

	for row, str := range grid {
		posNumAdjacentRolls = append(posNumAdjacentRolls, []int{})
		for col, currTile := range str {
			if currTile != '@' {
				continue
			}

			numAdjacent := 0
			for dRow := -1; dRow <= 1; dRow += 1 {
				for dCol := -1; dCol <= 1; dCol += 1 {
					if dRow == 0 && dCol == 0 {
						continue
					}
					if row+dRow >= 0 && col+dCol >= 0 && row+dRow <= len(grid)-1 && col+dCol <= len(str)-1 && grid[row+dRow][col+dCol] == '@' {
						numAdjacent += 1
					}
				}
			}

			posNumAdjacentRolls[row][col] = numAdjacent

			if numAdjacent < 4 {
				currentAccessibleRolls = append(currentAccessibleRolls, pos{row, col})
			}
		}
	}

	for len(currentAccessibleRolls) > 0 {
		accessiblePosition := currentAccessibleRolls[len(currentAccessibleRolls)-1]
		currentAccessibleRolls = currentAccessibleRolls[:len(currentAccessibleRolls)-1]

		totalAccessible += 1

		row := accessiblePosition.row
		col := accessiblePosition.col

		for dRow := -1; dRow <= 1; dRow += 1 {
			for dCol := -1; dCol <= 1; dCol += 1 {
				if dRow == 0 && dCol == 0 {
					continue
				}

				adjRow := row + dRow
				adjCol := col + dCol
				if adjRow >= 0 && adjCol >= 0 && adjRow <= len(grid)-1 && adjCol <= len(grid[0])-1 && grid[adjRow][adjCol] == '@' {
					if posNumAdjacentRolls[adjRow][adjCol] == 4 {
						currentAccessibleRolls = append(currentAccessibleRolls, pos{row: adjRow, col: adjCol})
					}

					posNumAdjacentRolls[adjRow][adjCol] -= 1
				}
			}
		}

	}

	return totalAccessible
}

func parseInput(filename string) []string {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}

	grid := []string{}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		grid = append(grid, line)
	}

	return grid
}

func main() {
	fmt.Println(part1(parseInput(os.Args[1])))
	fmt.Println(part2(parseInput(os.Args[1])))
}
