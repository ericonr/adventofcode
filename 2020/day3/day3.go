package main

import (
	"bufio"
	"fmt"
	"os"
	"log"
)

func main() {
	f, _ := os.Open("data")
	defer f.Close()

	// arbitrary max length
	first_line := make([]byte, 255)
	max_le, _ := f.Read(first_line)
	repeat_after := 0
	for i, c := range first_line {
		if c == '\n' {
			repeat_after = i
			break
		}
		if i == max_le {
			log.Fatal("line too long!")
		}
	}

	fmt.Printf("line length: %d\n", repeat_after)
	// arbitrary max lines
	themap := make([][]byte, 1000)
	for i := range themap {
		themap[i] = make([]byte, repeat_after)
	}

	f.Seek(0, 0)
	scanner := bufio.NewScanner(f)
	ln := 0
	for scanner.Scan() {
		copy(themap[ln], scanner.Bytes())
		ln += 1
	}

	// movement: right,down
	// 1,1 - 3,1 - 5,1 - 7,1
	counts := []int{0, 0, 0, 0}
	hoff := []int{1, 3, 5, 7}
	// 1,2
	count2 := 0
	// (v,h): vertical and horizontal
	for v := 0; v < ln; v++ {
		for i := range counts {
			h:= (v * hoff[i]) % repeat_after
			if themap[v][h] == '#' {
				counts[i]++
			}
		}

		if v % 2 == 0 {
			h := (v / 2) % repeat_after
			if themap[v][h] == '#' {
				count2++
			}
		}
	}

	// part 1
	fmt.Printf("total trees: %d\n", counts[1])
	// part 2
	mul := 1
	for _, v := range counts {
		mul *= v
	}
	mul *= count2
	fmt.Printf("product of total trees: %d\n", mul)
}
