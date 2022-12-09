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

var input_file string

func init() {
	flag.StringVar(&input_file, "f", "input.txt", "the input file")
	flag.Parse()
}

func main() {
	log.SetFlags(0)

	file, err := os.Open(input_file)
	if err != nil {
		log.Fatalf("Could not open file %s", input_file)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	path := []string{}
	dirs := make(map[string]int)

	i := 0
	for scanner.Scan() {
		i++
		line := scanner.Text()
		words := strings.Split(line, " ")

		if words[0] == "$" {
			if words[1] == "cd" {
				if words[2] == ".." {
					path = pop(path)
				} else {
					path = append(path, words[2])
				}
			}
		} else if words[0] != "dir" {
			filesize, err := strconv.Atoi(words[0])
			if err != nil {
				log.Fatal("could not read file size")
			}

			for i := 0; i < len(path); i++ {
				c := concatPath(path, i+1)

				if v, ok := dirs[c]; ok {
					dirs[c] = v + filesize
				} else {
					dirs[c] = filesize
				}
			}
		}
	}

	sum := 0
	for _, size := range dirs {
		if size <= 100_000 {
			sum += size
		}
	}
	fmt.Println(sum)

	required := 30_000_000 - (70_000_000 - dirs["/"])
	min := 100000000000000
	for _, size := range dirs {
		if size >= required && size < min {
			min = size
		}
	}
	fmt.Println(min)
}

func pop(path []string) []string {
	if len(path) == 0 {
		return path
	} else {
		return path[:len(path)-1]
	}
}

func concatPath(path []string, n int) string {
	if n == 1 {
		return "/"
	}

	acc := []byte{}
	for i := 1; i < n; i++ {
		acc = fmt.Appendf(acc, "%s/", path[i])
	}
	return string(acc)
}
