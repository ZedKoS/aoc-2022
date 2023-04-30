package main

import (
	"bufio"
	pq "day12/binary_heap"
	"day12/grid"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
)

type Pos = grid.Pos
type Heightmap = grid.Grid[int]

var (
	LEFT  = Pos{X: -1, Y: 0}
	RIGHT = Pos{X: 1, Y: 0}
	DOWN  = Pos{X: 0, Y: 1}
	UP    = Pos{X: 0, Y: -1}
)

var part int
var inputFile string

func init() {
	log.SetFlags(0)

	flag.IntVar(&part, "p", 1, "which part to solve")
	flag.StringVar(&inputFile, "f", "input.txt", "input file")
	flag.Parse()

	if part != 1 && part != 2 {
		log.Fatalf("invalid part: %d\n", part)
	}
}

func main() {
	heightmap := readHeightmap()

	if part == 1 {
		steps := findSteps(heightmap)
		fmt.Printf("End reached in %d steps\n", steps)
	} else {
		min := math.MaxInt

		for i, cell := range heightmap.Values() {
			if cell != int('a') {
				continue
			}

			heightmap.Start = heightmap.ToPos(i)
			steps := findSteps(heightmap)

			if steps != -1 && steps < min {
				min = steps
			}
		}

		fmt.Printf("MIN steps = %d\n", min)
	}
}

func readHeightmap() Heightmap {
	f, err := os.Open(inputFile)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	lines := bufio.NewScanner(f)

	raw_heightmap := []int{}
	var start, end Pos
	var row, col int

	for lines.Scan() {
		col = 0
		line := lines.Text()

		for _, c := range line {
			var v int

			switch c {
			case 'S':
				start = Pos{X: col, Y: row}
				v = int('a')
			case 'E':
				end = Pos{X: col, Y: row}
				v = int('z')
			default:
				v = int(c)
			}

			raw_heightmap = append(raw_heightmap, v)
			col++
		}

		row++
	}

	grid, err := grid.GridFrom(raw_heightmap, col, row)
	if err != nil {
		log.Fatal(err)
	}

	grid.Start = start
	grid.Finish = end
	return grid
}

func Neighbours(grid Heightmap, pos grid.Pos) []Pos {
	current_height := *grid.GetIndex(grid.ToIndex(pos))

	all := [4]Pos{
		{X: pos.X - 1, Y: pos.Y},
		{X: pos.X + 1, Y: pos.Y},
		{X: pos.X, Y: pos.Y - 1},
		{X: pos.X, Y: pos.Y + 1},
	}

	ns := make([]Pos, 0, 4)

	for _, n := range all {
		if height, err := grid.Get(n); err == nil {
			if *height-1 <= current_height {
				ns = append(ns, n)
			}
		}
	}

	return ns
}

func Astar(g Heightmap) []Pos {
	openSet := pq.Make[Pos](1)
	pq.Push[Pos](&openSet, g.Start, 0)

	cameFrom := make(map[Pos]Pos)

	costMap := make(map[Pos]int)
	costMap[g.Start] = 0

	getCost := func(pos Pos) int {
		if c, ok := costMap[pos]; ok {
			return c
		} else {
			return math.MaxInt
		}
	}

	for openSet.Len() != 0 {
		current := pq.Pop[Pos](&openSet)

		if current == g.Finish {
			return reconstructPath(cameFrom, current)
		}

		currentCost := getCost(current)

		for _, n := range Neighbours(g, current) {
			var newCost int
			if currentCost == math.MaxInt {
				newCost = math.MaxInt
			} else {
				newCost = currentCost + 1
			}

			if newCost >= getCost(n) {
				continue
			}

			// this path is better
			cameFrom[n] = current
			costMap[n] = newCost

			if !pq.Contains(openSet, n) {
				priority := newCost + grid.Dist(n, g.Finish)
				pq.Push[Pos](&openSet, n, priority)
			}
		}
	}

	return nil
}

func reconstructPath(cameFrom map[Pos]Pos, current Pos) []Pos {
	path := []Pos{current}
	for {
		if _, ok := cameFrom[current]; ok {
			current = cameFrom[current]
			path = append(path, current)
		} else {
			break
		}
	}

	for i, j := 0, len(path)-1; i < j; i, j = i+1, j-1 {
		path[i], path[j] = path[j], path[i]
	}
	return path
}

func findSteps(heightmap Heightmap) (steps int) {
	path := Astar(heightmap)

	if path == nil {
		return -1
	} else {
		return len(path) - 1
	}
}
