package binary_heap

import "sort"

type Interface[T any] interface {
	sort.Interface

	Push(value T, priority int)
	Peek() T
	Pop() T
}
type PriorityQueue[T any] Interface[T]

func Push[T any](h Interface[T], value T, priority int) {
	h.Push(value, priority)
	siftUp(h, h.Len()-1)
}

func Pop[T any](h Interface[T]) T {
	h.Swap(0, h.Len()-1)
	x := h.Pop()
	siftDown(h, 0)
	return x
}

//

type Item[T any] struct {
	data  T
	value int
}

type BinaryHeap[T any] []Item[T]

func Empty[T any]() BinaryHeap[T] {
	return make([]Item[T], 0)
}

func Make[T any](depth int) BinaryHeap[T] {
	els := 1<<depth - 1
	heap := make([]Item[T], 0, els)
	return heap
}

func (h BinaryHeap[T]) Less(i, j int) bool {
	return h[i].value < h[j].value
}

func (h BinaryHeap[T]) Len() int {
	return len(h)
}

func (h BinaryHeap[T]) Swap(i, j int) {
	h[i], h[j] = h[j], h[i]
}

func children(index int) int {
	return 2*index + 1
}

func parent(index int) int {
	return (index - 1) / 2
}

func isLeaf[T any](h Interface[T], index int) bool {
	c := children(index)
	return c >= h.Len()
}

func siftUp[T any](h Interface[T], index int) int {
	for index > 0 {
		pi := parent(index)

		// done
		if !h.Less(index, pi) {
			break
		}

		h.Swap(index, pi)
		index = pi
	}
	return index
}

func siftDown[T any](h Interface[T], index int) int {
	for !isLeaf(h, index) {
		ci := children(index)
		var sel int

		// get index of child with the lowest priority
		// if there's more than 1 child :)
		if ci+1 >= h.Len() || h.Less(ci, ci+1) {
			sel = ci
		} else {
			sel = ci + 1
		}

		// done
		if !h.Less(sel, index) {
			break
		}

		h.Swap(sel, index)
		index = sel
	}
	return index
}

func (h *BinaryHeap[T]) Push(value T, priority int) {
	it := Item[T]{value, priority}
	*h = append(*h, it)
}

// get item with the lowest priority
func (h BinaryHeap[T]) Peek() T {
	return h[0].data
}

func (h *BinaryHeap[T]) Pop() T {
	last := (*h)[h.Len()-1]
	*h = (*h)[:h.Len()-1] // shrink heap
	return last.data
}

func Contains[T comparable](h BinaryHeap[T], value T) bool {
	for _, v := range h {
		if value == v.data {
			return true
		}
	}
	return false
}
