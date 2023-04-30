package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

var part int
var inputFile string

func init() {
	log.SetFlags(0)

	flag.IntVar(&part, "p", 1, "which part to solve")
	flag.StringVar(&inputFile, "f", "input.txt", "input file")
	flag.Parse()

	if part != 1 && part != 2 {
		log.Fatalf("Invalid part: %d", part)
	}
}

type CPU struct {
	x          int
	pc         int
	cycles     int
	fix        bool
	halfx      int
	crtx, crty int
}

type Op struct {
	opcode int
	value  int
}

const (
	noop = iota
	addx = iota
)

func makeCPU() CPU {
	return CPU{
		x:      1,
		pc:     0,
		cycles: 0,
		fix:    false,
		halfx:  0,
		crtx:   0,
		crty:   0}
}

func main() {
	cpu := makeCPU()
	ops := readOps()
	screen := [6][40]bool{}

	switch part {
	case 1:
		halts := []int{0, 20, 60, 100, 140, 180, 220}
		acc := 0

		for i := 1; i < len(halts); i++ {
			halt := halts[i]
			prev := halts[i-1]
			runCPU(&cpu, &screen, ops, halt-prev-1)

			log.Printf("-- HALT %d [x = %d, cycle = %d]", halt, cpu.x, cpu.cycles+1)
			acc += cpu.x * halt

			runCPU(&cpu, &screen, ops, 1)
		}

		// runCPU(&cpu, ops, -1)

		log.Printf("Sum = %d\n", acc)

	case 2:
		runCPU(&cpu, &screen, ops, -1)

		f, err := os.Create("out.txt")
		if err != nil {
			log.Fatal(err)
		}
		defer f.Close()

		writer := bufio.NewWriter(f)
		defer writer.Flush()

		for i := 0; i < len(screen); i++ {
			for j := 0; j < len(screen[i]); j++ {
				if screen[i][j] {
					writer.WriteByte('#')
				} else {
					writer.WriteByte(' ')
				}
			}
			writer.WriteByte('\n')
		}

	}
}

func readOps() []Op {
	f, err := os.Open(inputFile)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	lines := bufio.NewScanner(f)
	ops := []Op{}

	for lines.Scan() {
		words := strings.Split(lines.Text(), " ")

		if words[0] == "noop" {
			ops = append(ops, Op{noop, 0})
		} else {
			value, err := strconv.Atoi(words[1])
			if err != nil {
				log.Fatal(err)
			}

			ops = append(ops, Op{addx, value})
		}
	}

	return ops
}

func runCPU(cpu *CPU, screen *[6][40]bool, ops []Op, cycles int) {
	for i := 0; i != cycles; i++ {
		fmt.Printf("(%d) [pc = %d, x = %d, hx = %d] | CRT [x = %d, y = %d]\n", cpu.cycles, cpu.pc, cpu.x, cpu.halfx, cpu.crtx, cpu.crty)

		var op Op
		if cpu.pc < len(ops) {
			op = ops[cpu.pc]
		} else {
			op = Op{noop, 0}
		}

		fixed := false

		// begin execution
		if !cpu.fix {
			switch op.opcode {
			case noop:
				cpu.pc++

			case addx:
				v := op.value
				cpu.halfx = cpu.x + v
				cpu.fix = true
				fixed = true
				// do not inc cpu.pc
			}
		}

		// crt
		if cpu.x-1 <= cpu.crtx && cpu.crtx <= cpu.x+1 {
			screen[cpu.crty][cpu.crtx] = true
		}

		cpu.crtx++
		if cpu.crtx >= 40 {
			cpu.crtx = 0
			cpu.crty++
		}

		if cpu.crty >= 6 {
			break
		}

		// end execution
		if cpu.fix && !fixed {
			cpu.x = cpu.halfx
			cpu.halfx = 0
			cpu.fix = false
			cpu.pc++
		}

		cpu.cycles++
	}
}
