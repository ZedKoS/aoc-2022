package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"strings"
)

const (
	ROWS = 200
	COLS = 1000
)

const (
	ROCK = '#'
	AIR  = 0
	SAND = '.'
)

var (
	UP    = Pos{0, -1}
	DOWN  = Pos{0, 1}
	LEFT  = Pos{-1, 0}
	RIGHT = Pos{1, 0}
)

var SPAWN = Pos{500, 0}

type Pos struct {
	x, y int
}
type Cave [ROWS][COLS]int

var inputFile string

func init() {
	log.SetFlags(0)

	flag.StringVar(&inputFile, "f", "input.txt", "input file")
	flag.Parse()
}

func main() {
	cave := readCave()

	i := 0
	for simSand(&cave) {
		i++
	}
	fmt.Println(i)

	out, err := os.Create("out.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer out.Close()

	writer := bufio.NewWriter(out)
	for i := 0; i < ROWS; i++ {
		for j := 0; j < COLS; j++ {
			if cave[i][j] == AIR {
				writer.WriteByte(' ')
			} else {
				writer.WriteByte(byte(cave[i][j]))
			}
		}
		writer.WriteByte('\n')
	}
}

func readCave() (cave Cave) {
	file, err := os.Open(inputFile)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	lines := bufio.NewScanner(file)
	maxy := 0

	for lines.Scan() {
		var pairs []Pos

		for _, pair_raw := range strings.Split(lines.Text(), " -> ") {
			var x, y int
			fmt.Sscanf(pair_raw, "%d,%d", &x, &y)
			pairs = append(pairs, Pos{x, y})

			if y > maxy {
				maxy = y
			}
		}

		for i := 1; i < len(pairs); i++ {
			point0 := pairs[i-1]
			point1 := pairs[i]
			drawLine(&cave, point0, point1)
		}
	}

	fmt.Printf("DEBUG: max y = %d\n", maxy)
	drawLine(&cave, Pos{0, maxy + 2}, Pos{COLS - 1, maxy + 2})

	return cave
}

func drawLine(cave *Cave, from, to Pos) {
	diff := Pos{to.x - from.x, to.y - from.y}

	if diff.x != 0 {
		s := signum(diff.x)

		for i := from.x; i != to.x; i += s {
			cave[to.y][i] = ROCK
		}
	}

	if diff.y != 0 {
		s := signum(diff.y)

		for i := from.y; i != to.y; i += s {
			cave[i][to.x] = ROCK
		}
	}

	cave[to.y][to.x] = ROCK
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

func simSand(cave *Cave) bool {
	if cave[SPAWN.y][SPAWN.x] != AIR {
		return false
	}

	pos := SPAWN
	next := pos

	for {
		// fmt.Println(next)
		next = Pos{pos.x, pos.y + 1}

		// check out of bounds
		if next.y >= ROWS || next.x < 0 || next.x >= COLS {
			break
		}

		if cave[next.y][next.x] == AIR {
			pos = next
			continue
		}

		next = Pos{pos.x - 1, pos.y + 1}

		// check out of bounds
		if next.y >= ROWS || next.x < 0 || next.x >= COLS {
			break
		}

		if cave[next.y][next.x] == AIR {
			pos = next
			continue
		}

		next = Pos{pos.x + 1, pos.y + 1}

		// check out of bounds
		if next.y >= ROWS || next.x < 0 || next.x >= COLS {
			break
		}

		if cave[next.y][next.x] == AIR {
			pos = next
			continue
		}

		break
	}

	cave[pos.y][pos.x] = SAND
	return true
}
