package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
)

var part int
var inputFile string
var knotsCount int

const (
	up    = 'U'
	down  = 'D'
	left  = 'L'
	right = 'R'
)

type Move struct {
	dir   byte
	steps int
}

type pos struct {
	x, y int
}

func init() {
	log.SetFlags(0)

	flag.IntVar(&part, "p", 1, "which part to run")
	flag.StringVar(&inputFile, "f", "input.txt", "input file")
	flag.IntVar(&knotsCount, "k", 0, "number of knots")

	flag.Parse()

	if part < 0 || part > 2 {
		log.Fatalf("invalid part: %d", part)
	}
}

func main() {
	moves := readMoves()

	k := knotsCount
	if k == 0 {
		switch part {
		case 1:
			k = 2
		case 2:
			k = 10
		}
	}

	grid := runMoves(moves, k)
	count := len(grid)
	fmt.Printf("number of cells occupied (%d knots): %d", k, count)
}

func readMoves() []Move {
	file, err := os.Open(inputFile)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	lines := bufio.NewScanner(file)
	moves := []Move{}

	for lines.Scan() {
		line := lines.Text()

		var move Move
		fmt.Sscanf(line, "%c %d", &move.dir, &move.steps)

		moves = append(moves, move)
	}

	return moves
}

func runMoves(moves []Move, knots_n int) map[pos]bool {
	if knots_n < 2 {
		return nil
	}

	knots := make([]pos, knots_n)
	head := &knots[0]

	visited := make(map[pos]bool)
	visited[*head] = true

	for _, move := range moves {
		delta := pos{0, 0}

		switch move.dir {
		case left:
			delta.x = -1
		case right:
			delta.x = 1
		case up:
			delta.y = 1
		case down:
			delta.y = -1
		}

		for s := 0; s < move.steps; s++ {
			head.x += delta.x
			head.y += delta.y

			for k := 1; k < len(knots); k++ {
				knots[k] = follow(knots[k], knots[k-1])
			}

			visited[knots[len(knots)-1]] = true
		}
	}

	return visited
}

func abs(x int) int {
	if x < 0 {
		x = -x
	}
	return x
}

func signum(x int) int {
	if x > 0 {
		return 1
	} else if x < 0 {
		return -1
	} else {
		return 0
	}
}

func follow(tail, head pos) (newTail pos) {
	diff := pos{head.x - tail.x, head.y - tail.y}
	newTail = tail

	if abs(diff.x) >= 2 || abs(diff.y) >= 2 {
		newTail.x += signum(diff.x)
		newTail.y += signum(diff.y)
	}

	return newTail
}
