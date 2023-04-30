package grid

import "errors"

type Pos struct {
	X, Y int
}

type Grid[T any] struct {
	values        []T
	width         int
	height        int
	Start, Finish Pos
}

// computes the manhattan distance between two points
func Dist(from, to Pos) int {
	xdist := from.X - to.X
	ydist := from.Y - to.Y

	if xdist < 0 {
		xdist *= -1
	}
	if ydist < 0 {
		ydist *= -1
	}

	return xdist + ydist
}

func Diff(to, from Pos) Pos {
	return Pos{X: to.X - from.X, Y: to.Y - from.Y}
}

func MakeGrid[T any](width, height int) (Grid[T], error) {
	grid := Grid[T]{nil, width, height, Pos{}, Pos{}}

	if width < 1 || height < 1 {
		return grid, errors.New("invalid grid dimensions")
	}

	grid.values = make([]T, width*height)

	return grid, nil
}

func GridFrom[T any](raw_grid []T, width, height int) (Grid[T], error) {
	grid := Grid[T]{nil, width, height, Pos{}, Pos{}}

	if width < 1 || height < 1 {
		return grid, errors.New("invalid grid dimensions")
	}

	if len(raw_grid) != grid.Size() {
		return grid, errors.New("invalid raw_grid size")
	}

	grid.values = raw_grid

	return grid, nil
}

func (grid Grid[T]) ToIndex(pos Pos) int {
	return pos.X + pos.Y*grid.width
}

func (grid Grid[T]) ToPos(index int) Pos {
	return Pos{X: index % grid.width, Y: index / grid.width}
}

func (grid Grid[T]) Get(pos Pos) (*T, error) {
	var value *T

	if pos.X < 0 || pos.X >= grid.width {
		return value, errors.New("invalid x coord")
	}
	if pos.Y < 0 || pos.Y >= grid.height {
		return value, errors.New("invalid y coord")
	}

	value = &grid.values[grid.ToIndex(pos)]
	return value, nil
}

func (grid Grid[T]) GetIndex(i int) *T {
	return &grid.values[i]
}

func (grid Grid[T]) Width() int {
	return grid.width
}

func (grid Grid[T]) Height() int {
	return grid.height
}

func (grid Grid[T]) Size() int {
	return grid.width * grid.height
}

func (grid Grid[T]) ValidPos(pos Pos) bool {
	if pos.X < 0 || pos.X >= grid.width {
		return false
	}

	if pos.Y < 0 || pos.Y >= grid.width {
		return false
	}

	return true
}

func (grid Grid[T]) Values() []T {
	valuesCopy := make([]T, len(grid.values))
	copy(valuesCopy, grid.values)
	return valuesCopy
}
