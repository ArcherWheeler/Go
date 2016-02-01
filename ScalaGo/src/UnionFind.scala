/**
*  Created by Archer Wheeler on 1/17/16.
*/

class UnionFind[T](_element: T){
  def element = _element
  var parent: UnionFind[T] = this

  def union(elementSet: UnionFind[T]): UnionFind[T] = {
    representative.parent = elementSet.representative
    elementSet.representative
  }

  def ==(elementSet: UnionFind[T]): Boolean = {
    representative.equals(elementSet.representative)
  }

  def representative: UnionFind[T] = {
    var r = this
    if (parent != this)
      r = parent.representative
    parent = r
    r
  }
}


abstract class Point {
  val cor: (Int,Int)
  var group: Group
}



//val board = Map[((Int,Int),Option[Group])]

//class Group {
//
//
//  var stones: Set[Point] = Set()
//
//
//
//
//
//}











object quickTest {
  def main(args: Array[String]): Unit = {
    val a = new UnionFind(1)
    val b = new UnionFind(2)
    val c = new UnionFind(3)
    val d = new UnionFind(4)

    a.union(b)
    b.union(c)

    println (a.representative.element)
    println (a == b)
    println (a == d)
  }
}