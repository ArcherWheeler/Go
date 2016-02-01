/**
 * Created by Archer Wheeler.
 */

class Group(_color: Int) {
  var color = _color
  var stones: Set[(Int,Int)] = Set()
  var liberties: Set[(Int,Int)] = Set()

  def containsStone(p: (Int,Int)) = stones.contains(p)

  def ++(g: Group): Group = {
    stones = stones ++ g.stones
    liberties = liberties ++ g.liberties
    this
  }

  def addStone(p: (Int,Int)): Unit = {
    stones += p
  }
  def removeLiberty(p: (Int,Int)): Unit = {
    liberties -= p
  }

  def canExpandTo(p: (Int,Int), c: Int): Boolean = {
    c == color && liberties.contains(p)
  }

  def isDead: Boolean = {
    liberties.isEmpty
  }
}

class Board(size: Int){


  var currentPlayerGroups: Set[Group] = Set()
  var opponentGroups: Set[Group] = Set()

  def groups: Set[Group] = currentPlayerGroups | opponentGroups

  var nextToMove: Int = 1
  var otherPlayer: Int = -1

  def getGroup(p: (Int,Int)): Option[Group] = {
    for(g <- groups) {
      if (g.stones.contains(p)) {
        return Some(g)
      }
    }
    None
  }

  def placeStone(p: (Int, Int)): Unit = {
    val newGroup: Group = new Group(nextToMove)
    newGroup.addStone(p)

    newGroup.liberties = getEmptyAdjacent(p)

    val adj = currentPlayerGroups.filter(g => g.canExpandTo(p, nextToMove))
    adj.foreach(g => newGroup ++ g)

    currentPlayerGroups += newGroup
    currentPlayerGroups = groups -- adj

    //Maybe optimize to just opponent? Potentially a gotcha
    groups.foreach(g => g.removeLiberty(p))



    val opponentDead = opponentGroups.filter(g => g.isDead)
    //remove dead stones
    opponentGroups = opponentGroups -- opponentDead

    //reclaim new liberties
    for (g <- opponentDead; s <- g.stones) {
      getAdjacentGroups(s).foreach(ad => ad.liberties += s)
    }


    //now remove your stones. Maybe remove and check before move?
    //Under some rule sets self capture is "illegal"

    val playerDead = currentPlayerGroups.filter(g => g.isDead)
    //remove dead stones
    currentPlayerGroups = currentPlayerGroups -- opponentDead

    //reclaim new liberties
    for (g <- playerDead; s <- g.stones) {
      getAdjacentGroups(s).foreach(ad => ad.liberties += s)
    }

    //swap players
    val hold = nextToMove
    nextToMove = otherPlayer
    otherPlayer = hold

    val holdg = currentPlayerGroups
    currentPlayerGroups = opponentGroups
    opponentGroups = holdg

  }

  def getCor(p: (Int,Int)): Int ={
    for (g <- groups){
      if (g.stones.contains(p)) return g.color
    }
    return 0
  }

  def onBoard(p: (Int,Int)): Boolean = {
    val (x,y) = p
    (x >= 0) && (x < size) && (y >= 0) && (y < size)
  }

  def getEmptyAdjacent(p: (Int,Int)): Set[(Int,Int)] = {
    val (x,y) = p
    var adjacent = Set((x-1,y),(x+1,y),(x,y-1),(x,y+1)).filter(onBoard)
    for (g <- groups) {
      adjacent = adjacent.filter(adj => !g.stones.contains(adj))
    }
    adjacent
  }

  def getAdjacentGroups(p: (Int,Int)): Set[Group] = {
    val (x,y) = p
    var adjacent = Set((x-1,y),(x+1,y),(x,y-1),(x,y+1)).filter(onBoard)
    groups.filter(g => g.stones.intersect(adjacent).nonEmpty)
  }

  def getSurrounding(p: (Int,Int)): Set[(Int,Int)] = {
    val (x,y) = p
    val surrounding = Set((x-1,y-1),(x-1,y),(x-1,y+1),
      (x,y-1),(x,y+1),
      (x+1,y-1),(x+1,y),(x+1,y+1))
    surrounding.filter(onBoard)
  }

  def seeIt: Unit = {
    for (y <- 0 to size-1){
      for (x <- 0 to size-1) {
        val v = getCor(x,y)
        if (v == 0) print ("-")
        if (v == 1) print ("X")
        if (v == -1) print ("0")
        print (" ")
      }
      println
    }
  }
}

object HelloWorld {
  def main(args: Array[String]): Unit = {
    var b = new Board(5)
    b.placeStone(0,1)

    b.placeStone(0,2)

    b.placeStone(1,0)

    b.placeStone(1,1)

    b.placeStone(0,4)
    b.placeStone(0,0)
    b.placeStone(0,1)
//
//    b.nextToMove = -1
//    b.otherPlayer = 1
//
//
//    b.placeStone(2,0)



    b.seeIt
  }
}
