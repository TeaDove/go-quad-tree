package qtree

import (
	"fmt"
	"math"
	"reflect"
	"sort"
)

// *****Defs********
type Tree[T any] interface {
	Insert(p *Point[T]) Tree[T]
	Remove(p *Point[T]) Tree[T]
	Bounds() *Bounds[T]
	CountPoints() int
	String() string
	isEmpty() bool
	isLeaf() bool
	Points() []Point[T]
	Find(p *Point[T]) Tree[T]
	Parent() Tree[T]
	QueryRange(b *Bounds[T]) []Point[T]
	GetLeaves() []Tree[T]
}

type Point[T any] struct {
	X, Y float64
	Val  T
}

type PointSorter[T any] struct {
	p     *Point[T]
	ps    []Point[T]
	pLess func(p1, p2 *Point[T]) bool
}

func makePointSorter[T any](p *Point[T], ps []Point[T]) *PointSorter[T] {
	f := func(p1, p2 *Point[T]) bool {
		return Dist(p, p1) < Dist(p, p2)
	}

	pointsorter := PointSorter[T]{
		p:     p,
		ps:    ps,
		pLess: f,
	}
	return &pointsorter
}

func (psort *PointSorter[T]) Len() int {
	return len(psort.ps)
}

func (psort *PointSorter[T]) Less(i, j int) bool {
	return psort.pLess(&psort.ps[i], &psort.ps[j])
}

func (psort *PointSorter[T]) Swap(i, j int) {
	psort.ps[i], psort.ps[j] = psort.ps[j], psort.ps[i]
}

type BoundsList[T any] []Bounds[T]

func (bl BoundsList[T]) Len() int { return len(bl) }

func (bl BoundsList[T]) Swap(i, j int) {
	bl[i], bl[j] = bl[j], bl[i]
}

func (bl BoundsList[T]) Less(i, j int) bool {
	return bl[i].Size() < bl[j].Size()
}

type Bounds[T any] struct {
	x, y, w, h float64
}

type Qtree[T any] struct {
	bounds Bounds[T]
	root   Tree[T]
	pMax   int
}

type node[T any] struct {
	qt       *Qtree[T]
	parent   Tree[T]
	bounds   Bounds[T]
	children [4]Tree[T]
}

type leaf[T any] struct {
	qt     *Qtree[T]
	parent Tree[T]
	bounds Bounds[T]
	ps     []Point[T]
}

func NewPoint[T any](x, y float64, val T) *Point[T] {
	p := Point[T]{X: x, Y: y, Val: val}
	return &p
}

func Dist[T any](p1, p2 *Point[T]) float64 {
	diffx := p1.X - p2.X
	diffy := p1.Y - p2.Y
	return math.Sqrt(diffx*diffx + diffy*diffy)
}

// **********Bounds methods*****************
func NewBounds[T any](x, y, w, h float64) *Bounds[T] {
	b := Bounds[T]{x, y, w, h}
	return &b
}

func (b *Bounds[T]) Size() float64 {
	return (b.w - b.x) * (b.h - b.y)
}

func (b *Bounds[T]) quads() []*Bounds[T] {
	x, y, w, h := b.x, b.y, b.w/2.0, b.h/2.0
	nw := NewBounds[T](x, y, w, h)
	ne := NewBounds[T](x+w, y, w, h)
	sw := NewBounds[T](x, y+h, w, h)
	se := NewBounds[T](x+w, y+h, w, h)
	return []*Bounds[T]{nw, ne, sw, se}
}

func (b *Bounds[T]) contains(p *Point[T]) bool {
	if p.X <= b.x || p.X > b.x+b.w {
		return false
	}

	if p.Y <= b.y || p.Y > b.y+b.h {
		return false
	}
	return true
}

func intersect[T any](b1, b2 *Bounds[T]) bool {
	if b1.x+b1.w < b2.x {
		return false
	}

	if b2.x+b2.w < b1.x {
		return false
	}

	if b1.y+b1.h < b2.y {
		return false
	}

	if b2.y+b2.h < b1.y {
		return false
	}

	return true
}

//**********Qtree methods*****************

func NewQtree[T any](x, y, w, h float64, pMax int) *Qtree[T] {
	b := NewBounds[T](x, y, w, h)
	qt := Qtree[T]{
		bounds: *b,
		pMax:   pMax,
	}
	qt.root = newLeaf(&qt, &qt, b)
	return &qt
}

func (qt *Qtree[T]) GetLeaves() []Tree[T] { return qt.root.GetLeaves() }

func (qt *Qtree[T]) isLeaf() bool { return false }

func (qt *Qtree[T]) Points() []Point[T] { return qt.root.Points() }

func (qt *Qtree[T]) Parent() Tree[T] { return qt.root }

func (qt *Qtree[T]) Bounds() *Bounds[T] { return &qt.bounds }

func (qt *Qtree[T]) CountPoints() int { return qt.root.CountPoints() }

func (qt *Qtree[T]) Insert(p *Point[T]) Tree[T] {
	if !qt.bounds.contains(p) {
		msg := fmt.Sprintf("Point: %#v not in Bounds:%#v", p, qt.bounds)
		panic(msg)
		return nil
	}
	qt.root = qt.root.Insert(p)
	return qt
}

func (qt *Qtree[T]) isEmpty() bool { return qt.root.isEmpty() }

func (qt *Qtree[T]) String() string {
	str := fmt.Sprintf("pMax: %d, rootType: %v, Bounds: %#v, Root...\n", qt.pMax, reflect.TypeOf(qt.root), qt.bounds)
	str = fmt.Sprintf("%s%s", str, qt.root.String())
	return str
}

func (qt *Qtree[T]) Remove(p *Point[T]) Tree[T] {
	qt.root = qt.root.Remove(p)
	return qt
}

func (qt *Qtree[T]) Find(p *Point[T]) Tree[T] {
	return qt.root.Find(p)
}

func (qt *Qtree[T]) Neighbors(p *Point[T]) []Point[T] {
	tree := qt.Find(p)
	parent := tree.Parent().Parent()
	return parent.Points()
}

func nearest[T any](p *Point[T], ps []Point[T]) *Point[T] {
	if len(ps) == 1 {
		fmt.Println("nearest is only self")
		return &ps[0]
	}

	ndist := math.Inf(1)
	var nn Point[T]

	for _, n := range ps {
		d := Dist(p, &n)
		if d < ndist && !(p.X == n.X && p.Y == n.Y) {
			nn = n
			ndist = d
		}
	}
	return &nn
}

func (qt *Qtree[T]) NN(p *Point[T]) *Point[T] {
	tree := qt.Find(p)
	ps := tree.Points()
	b := tree.Bounds()

	if len(ps) < 2 {
		nbx := b.x - b.w - 1.0
		nby := b.y - b.h - 1.0
		nbw := b.w*3.0 + 1.0
		nbh := b.h*3.0 + 1.0
		nb := NewBounds[T](nbx, nby, nbw, nbh)
		qps := qt.QueryRange(nb)
		return nearest(p, qps)
	}
	localn := nearest(p, ps)
	d := Dist(p, localn) + 1.0
	nbx := p.X - d
	nby := p.Y - d
	nbw := d * 2.0
	nbh := d * 2.0
	nb := NewBounds[T](nbx, nby, nbw, nbh)
	qps := qt.QueryRange(nb)
	return nearest(p, qps)
}

func (qt *Qtree[T]) KNN(p *Point[T], k int) []Point[T] {
	tree := qt.Find(p)
	ps := tree.Points()
	if len(ps) > k {
		pscopy := make([]Point[T], len(tree.Points()))
		copy(pscopy, ps)
		tempkps := knearest(p, pscopy, k)
		//fmt.Println("tempkps\n", tempkps)
		d := Dist(p, &tempkps[len(tempkps)-1]) + 1.0
		nbx := p.X - d
		nby := p.Y - d
		nbw := d * 2.0
		nbh := d * 2.0
		nb := NewBounds[T](nbx, nby, nbw, nbh)
		qps := qt.QueryRange(nb)
		rpoints := knearest(p, qps, k)
		//fmt.Println("rpoints\n", rpoints)
		return rpoints
	}

	b := tree.Bounds()
	nbx := b.x - b.w - 1.0
	nby := b.y - b.h - 1.0
	nbw := b.w*3.0 + 1
	nbh := b.h*3.0 + 1
	nb := NewBounds[T](nbx, nby, nbw, nbh)
	points := qt.QueryRange(nb)
	rpoints := knearest(p, points, k)
	return rpoints
}

func knearest[T any](p *Point[T], ps []Point[T], k int) []Point[T] {
	for i, point := range ps {
		if point.X == p.X && point.Y == p.Y {
			ps = append(ps[:i], ps[i+1:]...)
		}
	}
	psort := makePointSorter(p, ps)
	sort.Sort(psort)
	if k > len(psort.ps) {
		k = len(psort.ps)
	}
	kpoints := psort.ps[0:k]
	rpoints := make([]Point[T], k)
	for i, pss := range kpoints {
		rpoints[i] = pss
	}

	return rpoints
}

type TreeList[T any] []Tree[T]

func (tl TreeList[T]) Len() int { return len(tl) }
func (tl TreeList[T]) Less(i, j int) bool {
	return tl[i].Bounds().Size() < tl[j].Bounds().Size()
}
func (tl TreeList[T]) Swap(i, j int) {
	tl[i], tl[j] = tl[j], tl[i]
}

func (qt *Qtree[T]) LargestLeaves(n int) []Tree[T] {
	leaves := TreeList[T](qt.GetLeaves())
	sort.Sort(leaves)
	rleaves := make([]Tree[T], n)
	copy(rleaves, leaves[0:n])
	return rleaves
}

func (qt *Qtree[T]) QueryRange(b *Bounds[T]) []Point[T] {
	return qt.root.QueryRange(b)
}

// *******node methods***********************
func newNode[T any](qt *Qtree[T], parent Tree[T], b *Bounds[T]) *node[T] {
	n := node[T]{
		qt:     qt,
		parent: parent,
		bounds: *b,
	}
	n.children = *newLeaves(qt, &n, b)
	return &n
}

func (n *node[T]) GetLeaves() []Tree[T] {
	leaves := make([]Tree[T], 0, 8)
	for _, c := range n.children {
		if c.isLeaf() {
			leaves = append(leaves, c.GetLeaves()...)
		}
	}
	return leaves
}

func (n *node[T]) String() string {
	str := fmt.Sprintf("Type: %v, Bounds: %#v, Children...\n", reflect.TypeOf(*n), n.bounds)
	for _, c := range n.children {
		str = fmt.Sprintf("%s%s", str, c.String())
	}
	return str
}

func (n *node[T]) Insert(p *Point[T]) Tree[T] {
	if !n.bounds.contains(p) {
		msg := fmt.Sprintf("Point: %#v not in Bounds:%#v", p, n.bounds)
		panic(msg)
		return nil
	}

	for i, c := range n.children {
		if c.Bounds().contains(p) {
			n.children[i] = c.Insert(p)
			return n
		}
	}

	msg := fmt.Sprintf("Didn't find a place for point %#v", p)
	panic(msg)
	return nil
}

func (n *node[T]) Remove(p *Point[T]) Tree[T] {
	for i, c := range n.children {
		b := c.Bounds()
		if b.contains(p) {
			n.children[i] = c.Remove(p)
			break
		}
	}

	allEmpty := true
	allLeaves := true
	points := make([]Point[T], 0, 4*n.qt.pMax)
	for _, c := range n.children {
		if !c.isEmpty() {
			allEmpty = false
		}
		if !c.isLeaf() {
			allLeaves = false
		} else {
			points = append(points, c.Points()...)
		}
	}

	if allEmpty {
		var l Tree[T]
		l = newLeaf[T](n.qt, n.parent, &n.bounds)
		return l
	}

	if allLeaves && len(points) <= n.qt.pMax {
		var l Tree[T]
		l = newLeaf[T](n.qt, n.parent, &n.bounds)
		for _, point := range points {
			l.Insert(&point)
		}
		return l
	}
	return n
}

func (n *node[T]) QueryRange(b *Bounds[T]) []Point[T] {
	points := make([]Point[T], 0, 4*n.qt.pMax)

	for _, c := range n.children {
		bc := c.Bounds()
		if intersect(b, bc) {
			points = append(points, c.QueryRange(b)...)
		}
	}
	return points
}

func (n *node[T]) Bounds() *Bounds[T] { return &n.bounds }

func (n *node[T]) CountPoints() int {
	count := 0
	for _, c := range n.children {
		count += c.CountPoints()
	}
	return count
}

func (n *node[T]) isEmpty() bool { return false }

func (n *node[T]) isLeaf() bool { return false }

func (n *node[T]) Points() []Point[T] {
	points := make([]Point[T], 0, n.qt.pMax*4)
	for _, c := range n.children {
		points = append(points, c.Points()...)
	}
	return points
}

func (n *node[T]) Find(p *Point[T]) Tree[T] {
	if !n.bounds.contains(p) {
		msg := fmt.Sprintf("Bounds error. Bounds: %#v, Point: %#v", n.bounds, p)
		panic(msg)
	}

	for _, c := range n.children {
		b := c.Bounds()
		if b.contains(p) {
			return c.Find(p)
		}
	}
	return nil
}

func (n *node[T]) Parent() Tree[T] {
	return n.parent
}

// ********leaf methods****************
func newLeaf[T any](qt *Qtree[T], parent Tree[T], bounds *Bounds[T]) *leaf[T] {
	l := leaf[T]{
		qt:     qt,
		parent: parent,
		bounds: *bounds,
		ps:     make([]Point[T], 0, qt.pMax),
	}
	return &l
}

func (l *leaf[T]) String() string {
	str := fmt.Sprintf("\nType: %v, Bounds: %#v, Points: %v", reflect.TypeOf(l), l.bounds, l.ps)
	return str
}

func newLeaves[T any](qt *Qtree[T], par *node[T], b *Bounds[T]) *[4]Tree[T] {
	bs := b.quads()
	var ls [4]Tree[T]
	for i, b4th := range bs {
		ls[i] = newLeaf[T](qt, par, b4th)
	}
	return &ls
}

func (l *leaf[T]) Insert(p *Point[T]) Tree[T] {
	if !l.bounds.contains(p) {
		msg := fmt.Sprintf("Point: %#v not in Bounds:%#v", p, l.bounds)
		panic(msg)
		return nil
	}
	//still room in bucket
	if len(l.ps) < l.qt.pMax {
		l.ps = append(l.ps, *p)
		return l
	}
	//insertion will overflow bucket. Make l into n, and reinsert pts
	var n Tree[T]
	n = newNode(l.qt, l.parent, &l.bounds)
	for _, pri := range l.ps {
		n = n.Insert(&pri)
	}
	n.Insert(p)

	//fmt.Println("Created New Node\n", n, "\nNew Node end")

	return n
}

func (l *leaf[T]) Remove(p *Point[T]) Tree[T] {
	end := len(l.ps) - 1
	for i := range l.ps {
		if &l.ps[i] == p {
			l.ps[i] = l.ps[end]
			l.ps = l.ps[0:end]
			break
		}
	}
	return l
}

func (l *leaf[T]) CountPoints() int {
	return len(l.ps)
}

func (l *leaf[T]) QueryRange(b *Bounds[T]) []Point[T] {
	points := make([]Point[T], 0, len(l.ps))
	for _, p := range l.ps {
		if b.contains(&p) {
			points = append(points, p)
		}
	}
	return points
}

func (l *leaf[T]) Bounds() *Bounds[T] { return &l.bounds }

func (l *leaf[T]) isEmpty() bool { return len(l.ps) == 0 }

func (l *leaf[T]) isLeaf() bool { return true }

func (l *leaf[T]) Points() []Point[T] { return l.ps }

func (l *leaf[T]) Find(p *Point[T]) Tree[T] {
	if l.bounds.contains(p) {
		return l
	}
	return nil
}

func (l *leaf[T]) GetLeaves() []Tree[T] { return nil }
func (l *leaf[T]) Parent() Tree[T]      { return l.parent }
