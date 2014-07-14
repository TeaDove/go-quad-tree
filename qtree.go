package qtree

import (
	"fmt"
	"math"
	"reflect"
	"sort"
)

//*****Defs********
type Tree interface {
	Insert(p *Point) Tree
	Remove(p *Point) Tree
	Bounds() *Bounds
	CountPoints() int
	String() string
	isEmpty() bool
	isLeaf() bool
	Points() []Point
	Find(p *Point) Tree
	Parent() Tree
	QueryRange(b *Bounds) []Point
	GetLeaves() []Tree
}

type Point struct {
	x, y float64
	Val  interface{}
}

type PointSorter struct {
	p     *Point
	ps    []Point
	pLess func(p1, p2 *Point) bool
}

func makePointSorter(p *Point, ps []Point) *PointSorter {
	f := func(p1, p2 *Point) bool {
		return Dist(p, p1) < Dist(p, p2)
	}

	pointsorter := PointSorter{
		p:     p,
		ps:    ps,
		pLess: f,
	}
	return &pointsorter
}

func (psort *PointSorter) Len() int {
	return len(psort.ps)
}

func (psort *PointSorter) Less(i, j int) bool {
	return psort.pLess(&psort.ps[i], &psort.ps[j])
}

func (psort *PointSorter) Swap(i, j int) {
	psort.ps[i], psort.ps[j] = psort.ps[j], psort.ps[i]
}

type BoundsList []Bounds

func (bl BoundsList) Len() int { return len(bl) }

func (bl BoundsList) Swap(i, j int) {
	bl[i], bl[j] = bl[j], bl[i]
}

func (bl BoundsList) Less(i, j int) bool {
	return bl[i].Size() < bl[j].Size()
}

type Bounds struct {
	x, y, w, h float64
}

type Qtree struct {
	bounds Bounds
	root   Tree
	pMax   int
}

type node struct {
	qt       *Qtree
	parent   Tree
	bounds   Bounds
	children [4]Tree
}

type leaf struct {
	qt     *Qtree
	parent Tree
	bounds Bounds
	ps     []Point
}

func NewPoint(x, y float64, val interface{}) *Point {
	p := Point{x: x, y: y, Val: val}
	return &p
}

func Dist(p1, p2 *Point) float64 {
	diffx := p1.x - p2.x
	diffy := p1.y - p2.y
	return math.Sqrt(diffx*diffx + diffy*diffy)
}

//**********Bounds methods*****************
func NewBounds(x, y, w, h float64) *Bounds {
	b := Bounds{x, y, w, h}
	return &b
}

func (b *Bounds) Size() float64 {
	return (b.w - b.x) * (b.h - b.y)
}

func (b *Bounds) quads() []*Bounds {
	x, y, w, h := b.x, b.y, b.w/2.0, b.h/2.0
	nw := NewBounds(x, y, w, h)
	ne := NewBounds(x+w, y, w, h)
	sw := NewBounds(x, y+h, w, h)
	se := NewBounds(x+w, y+h, w, h)
	return []*Bounds{nw, ne, sw, se}
}

func (b *Bounds) contains(p *Point) bool {
	if p.x <= b.x || p.x > b.x+b.w {
		return false
	}

	if p.y <= b.y || p.y > b.y+b.h {
		return false
	}
	return true
}

func intersect(b1, b2 *Bounds) bool {
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

func NewQtree(x, y, w, h float64, pMax int) *Qtree {
	b := NewBounds(x, y, w, h)
	qt := Qtree{
		bounds: *b,
		pMax:   pMax,
	}
	qt.root = newLeaf(&qt, &qt, b)
	return &qt
}

func (qt *Qtree) GetLeaves() []Tree { return qt.root.GetLeaves() }

func (qt *Qtree) isLeaf() bool { return false }

func (qt *Qtree) Points() []Point { return qt.root.Points() }

func (qt *Qtree) Parent() Tree { return qt.root }

func (qt *Qtree) Bounds() *Bounds { return &qt.bounds }

func (qt *Qtree) CountPoints() int { return qt.root.CountPoints() }

func (qt *Qtree) Insert(p *Point) Tree {
	if !qt.bounds.contains(p) {
		msg := fmt.Sprintf("Point: %#v not in Bounds:%#v", p, qt.bounds)
		panic(msg)
		return nil
	}
	qt.root = qt.root.Insert(p)
	return qt
}

func (qt *Qtree) isEmpty() bool { return qt.root.isEmpty() }

func (qt *Qtree) String() string {
	str := fmt.Sprintf("pMax: %d, rootType: %v, Bounds: %#v, Root...\n", qt.pMax, reflect.TypeOf(qt.root), qt.bounds)
	str = fmt.Sprintf("%s%s", str, qt.root.String())
	return str
}

func (qt *Qtree) Remove(p *Point) Tree {
	qt.root = qt.root.Remove(p)
	return qt
}

func (qt *Qtree) Find(p *Point) Tree {
	return qt.root.Find(p)
}

func (qt *Qtree) Neighbors(p *Point) []Point {
	tree := qt.Find(p)
	parent := tree.Parent().Parent()
	return parent.Points()
}

func nearest(p *Point, ps []Point) *Point {
	if len(ps) == 1 {
		fmt.Println("nearest is only self")
		return &ps[0]
	}

	ndist := math.Inf(1)
	var nn Point

	for _, n := range ps {
		d := Dist(p, &n)
		if d < ndist && !(p.x == n.x && p.y == n.y) {
			nn = n
			ndist = d
		}
	}
	return &nn
}

func (qt *Qtree) NN(p *Point) *Point {
	tree := qt.Find(p)
	ps := tree.Points()
	b := tree.Bounds()

	if len(ps) < 2 {
		nbx := b.x - b.w - 1.0
		nby := b.y - b.h - 1.0
		nbw := b.w*3.0 + 1.0
		nbh := b.h*3.0 + 1.0
		nb := NewBounds(nbx, nby, nbw, nbh)
		qps := qt.QueryRange(nb)
		return nearest(p, qps)
	}
	localn := nearest(p, ps)
	d := Dist(p, localn) + 1.0
	nbx := p.x - d
	nby := p.y - d
	nbw := d * 2.0
	nbh := d * 2.0
	nb := NewBounds(nbx, nby, nbw, nbh)
	qps := qt.QueryRange(nb)
	return nearest(p, qps)
}

func (qt *Qtree) KNN(p *Point, k int) []Point {
	tree := qt.Find(p)
	ps := tree.Points()
	if len(ps) > k {
		pscopy := make([]Point, len(tree.Points()))
		copy(pscopy, ps)
		tempkps := knearest(p, pscopy, k)
		//fmt.Println("tempkps\n", tempkps)
		d := Dist(p, &tempkps[len(tempkps)-1]) + 1.0
		nbx := p.x - d
		nby := p.y - d
		nbw := d * 2.0
		nbh := d * 2.0
		nb := NewBounds(nbx, nby, nbw, nbh)
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
	nb := NewBounds(nbx, nby, nbw, nbh)
	points := qt.QueryRange(nb)
	rpoints := knearest(p, points, k)
	return rpoints
}

func knearest(p *Point, ps []Point, k int) []Point {
	for i, point := range ps {
		if point.x == p.x && point.y == p.y {
			ps = append(ps[:i], ps[i+1:]...)
		}
	}
	psort := makePointSorter(p, ps)
	sort.Sort(psort)
	if k > len(psort.ps) {
		k = len(psort.ps)
	}
	kpoints := psort.ps[0:k]
	rpoints := make([]Point, k)
	for i, pss := range kpoints {
		rpoints[i] = pss
	}

	return rpoints
}

type TreeList []Tree

func (tl TreeList) Len() int { return len(tl) }
func (tl TreeList) Less(i, j int) bool {
	return tl[i].Bounds().Size() < tl[j].Bounds().Size()
}
func (tl TreeList) Swap(i, j int) {
	tl[i], tl[j] = tl[j], tl[i]
}

func (qt *Qtree) LargestLeaves(n int) []Tree {
	leaves := TreeList(qt.GetLeaves())
	sort.Sort(leaves)
	rleaves := make([]Tree, n)
	copy(rleaves, leaves[0:n])
	return rleaves
}

func (qt *Qtree) QueryRange(b *Bounds) []Point {
	return qt.root.QueryRange(b)
}

//*******node methods***********************
func newNode(qt *Qtree, parent Tree, b *Bounds) *node {
	n := node{
		qt:     qt,
		parent: parent,
		bounds: *b,
	}
	n.children = *newLeaves(qt, &n, b)
	return &n
}

func (n *node) GetLeaves() []Tree {
	leaves := make([]Tree, 0, 8)
	for _, c := range n.children {
		if c.isLeaf() {
			leaves = append(leaves, c.GetLeaves()...)
		}
	}
	return leaves
}

func (n *node) String() string {
	str := fmt.Sprintf("Type: %v, Bounds: %#v, Children...\n", reflect.TypeOf(*n), n.bounds)
	for _, c := range n.children {
		str = fmt.Sprintf("%s%s", str, c.String())
	}
	return str
}

func (n *node) Insert(p *Point) Tree {
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

func (n *node) Remove(p *Point) Tree {
	for i, c := range n.children {
		b := c.Bounds()
		if b.contains(p) {
			n.children[i] = c.Remove(p)
			break
		}
	}

	allEmpty := true
	allLeaves := true
	points := make([]Point, 0, 4*n.qt.pMax)
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
		var l Tree
		l = newLeaf(n.qt, n.parent, &n.bounds)
		return l
	}

	if allLeaves && len(points) <= n.qt.pMax {
		var l Tree
		l = newLeaf(n.qt, n.parent, &n.bounds)
		for _, point := range points {
			l.Insert(&point)
		}
		return l
	}
	return n
}

func (n *node) QueryRange(b *Bounds) []Point {
	points := make([]Point, 0, 4*n.qt.pMax)

	for _, c := range n.children {
		bc := c.Bounds()
		if intersect(b, bc) {
			points = append(points, c.QueryRange(b)...)
		}
	}
	return points
}

func (n *node) Bounds() *Bounds { return &n.bounds }

func (n *node) CountPoints() int {
	count := 0
	for _, c := range n.children {
		count += c.CountPoints()
	}
	return count
}

func (n *node) isEmpty() bool { return false }

func (n *node) isLeaf() bool { return false }

func (n *node) Points() []Point {
	points := make([]Point, 0, n.qt.pMax*4)
	for _, c := range n.children {
		points = append(points, c.Points()...)
	}
	return points
}

func (n *node) Find(p *Point) Tree {
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

func (n *node) Parent() Tree {
	return n.parent
}

//********leaf methods****************
func newLeaf(qt *Qtree, parent Tree, bounds *Bounds) *leaf {
	l := leaf{
		qt:     qt,
		parent: parent,
		bounds: *bounds,
		ps:     make([]Point, 0, qt.pMax),
	}
	return &l
}

func (l *leaf) String() string {
	str := fmt.Sprintf("\nType: %v, Bounds: %#v, Points: %v", reflect.TypeOf(l), l.bounds, l.ps)
	return str
}

func newLeaves(qt *Qtree, par *node, b *Bounds) *[4]Tree {
	bs := b.quads()
	var ls [4]Tree
	for i, b4th := range bs {
		ls[i] = newLeaf(qt, par, b4th)
	}
	return &ls
}

func (l *leaf) Insert(p *Point) Tree {
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
	var n Tree
	n = newNode(l.qt, l.parent, &l.bounds)
	for _, pri := range l.ps {
		n = n.Insert(&pri)
	}
	n.Insert(p)

	//fmt.Println("Created New Node\n", n, "\nNew Node end")

	return n
}

func (l *leaf) Remove(p *Point) Tree {
	end := len(l.ps) - 1
	for i := range l.ps {
		if l.ps[i] == *p {
			l.ps[i] = l.ps[end]
			l.ps = l.ps[0:end]
			break
		}
	}
	return l
}

func (l *leaf) CountPoints() int {
	return len(l.ps)
}

func (l *leaf) QueryRange(b *Bounds) []Point {
	points := make([]Point, 0, len(l.ps))
	for _, p := range l.ps {
		if b.contains(&p) {
			points = append(points, p)
		}
	}
	return points
}

func (l *leaf) Bounds() *Bounds { return &l.bounds }

func (l *leaf) isEmpty() bool { return len(l.ps) == 0 }

func (l *leaf) isLeaf() bool { return true }

func (l *leaf) Points() []Point { return l.ps }

func (l *leaf) Find(p *Point) Tree {
	if l.bounds.contains(p) {
		return l
	}
	return nil
}

func (l *leaf) GetLeaves() []Tree { return nil }
func (l *leaf) Parent() Tree      { return l.parent }
