package qtree

import (
	"fmt"
	"testing"
)

func Test1(*testing.T) {
	qt := NewQtree[uint](0.0, 0.0, 100.0, 100.0, 3)
	p1 := Point[uint]{5.0, 6.0, 3849}
	p2 := Point[uint]{51.0, 52.0, 24234}
	p3 := Point[uint]{54.0, 42.0, 34332}
	p4 := Point[uint]{6.0, 57.0, 323423}
	err := qt.Insert(&p1)
	err = qt.Insert(&p2)
	err = qt.Insert(&p3)
	err = qt.Insert(&p4)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(p1)
	fmt.Println(qt)
	b := Bounds[uint]{0, 0, 200, 200}
	pts := qt.QueryRange(&b)
	fmt.Println(pts)
}
