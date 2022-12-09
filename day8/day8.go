package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
)

const (
	LEFT  = iota
	RIGHT = iota
	UP    = iota
	DOWN  = iota
)

var part int
var input_file string
var workers int

func init() {
	flag.IntVar(&part, "p", 1, "select puzzle part: either 1 or 2")
	flag.StringVar(&input_file, "f", "input.txt", "the input file")
	flag.IntVar(&workers, "w", 1, "worker routines that solve the puzzle concurrently (1 or 2 is best)")

	flag.Parse()

	if part != 1 && part != 2 {
		log.Fatalf("invalid part: %d", part)
	}

	if workers <= 0 {
		log.Fatalf("invalid number of workers: %d", workers)
	}
}

func main() {
	log.SetFlags(0)

	forest := ReadForest()

	if part == 1 {
		SetVisibility(forest)

		visibleCount := 0
		for i := 0; i < len(forest); i++ {
			for j := 0; j < len(forest[i]); j++ {
				if forest[i][j].visible {
					visibleCount++
				}
			}
		}

		fmt.Printf("Visible trees: %d\n", visibleCount)
	} else {
		best := SetScore(forest)

		fmt.Printf("The best score is: %d\n", best)
	}
}

type Tree struct {
	height  int
	visible bool
	score   [4]int
}

func ReadForest() [][]Tree {
	f, err := os.Open(input_file)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	var forest [][]Tree

	lines := bufio.NewScanner(f)
	for lines.Scan() {
		var trees []Tree

		for _, c := range lines.Text() {
			height := int(c) - '0'
			trees = append(trees, Tree{height: height, visible: false, score: [4]int{}})
		}

		forest = append(forest, trees)
	}

	return forest
}

func SetVisibility(forest [][]Tree) {
	if len(forest) == 0 {
		return
	}

	// set corners

	for i := 0; i < len(forest); i++ {
		forest[i][0].visible = true
		forest[i][len(forest[i])-1].visible = true
	}

	for j := 0; j < len(forest[0]); j++ {
		forest[0][j].visible = true
		forest[len(forest)-1][j].visible = true
	}

	// rows

	for i := 1; i < len(forest)-1; i++ {
		highest_left := forest[i][0].height
		highest_right := forest[i][len(forest[i])-1].height

		for j := 1; j < len(forest[i])-1; j++ {
			tree_left := &forest[i][j]
			tree_right := &forest[i][len(forest[i])-1-j]

			if tree_left.height > highest_left {
				highest_left = tree_left.height
				tree_left.visible = true
			}

			if tree_right.height > highest_right {
				highest_right = tree_right.height
				tree_right.visible = true
			}
		}
	}

	// cols

	for j := 1; j < len(forest[0])-1; j++ {
		highest_up := forest[0][j].height
		highest_down := forest[len(forest)-1][j].height

		for i := 1; i < len(forest); i++ {
			tree_up := &forest[i][j]
			tree_down := &forest[len(forest)-1-i][j]

			if tree_up.height > highest_up {
				highest_up = tree_up.height
				tree_up.visible = true
			}

			if tree_down.height > highest_down {
				highest_down = tree_down.height
				tree_down.visible = true
			}
		}
	}
}

func SetScore(forest [][]Tree) (best int) {
	ch := make(chan int, workers)

	for w := 0; w < workers; w++ {
		go func(id int) {
			for i := 1; i < len(forest)-1; i++ {
				for j := 1 + id; j < len(forest[i])-1; j += workers {
					score := CalcScore(forest, i, j)
					ch <- score
				}
			}
		}(w)
	}

	best = 0
	for i := 1; i < len(forest)-1; i++ {
		for j := 1; j < len(forest[i])-1; j++ {
			score := <-ch
			if score > best {
				best = score
			}
		}
	}

	return best
}

func CalcScore(forest [][]Tree, row, col int) int {
	mytree := &forest[row][col]

	// up
	for i := row - 1; i >= 0; i-- {
		mytree.score[UP]++

		if forest[i][col].height >= mytree.height {
			break
		}
	}

	// down
	for i := row + 1; i < len(forest); i++ {
		mytree.score[DOWN]++

		if forest[i][col].height >= mytree.height {
			break
		}
	}

	// left
	for j := col - 1; j >= 0; j-- {
		mytree.score[LEFT]++

		if forest[row][j].height >= mytree.height {
			break
		}
	}

	// right
	for j := col + 1; j < len(forest[row]); j++ {
		mytree.score[RIGHT]++

		if forest[row][j].height >= mytree.height {
			break
		}
	}

	score := mytree.score[LEFT] * mytree.score[RIGHT] * mytree.score[UP] * mytree.score[DOWN]
	return score
}
