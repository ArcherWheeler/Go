/**
 * Created by Archer Wheeler on 2/1/16.
 */


class NaiveBoard(size: Int, currentPlayer: Int = 1, ko: Option[(Int,Int)] = None) {

  val board = Array.ofDim[Int](size,size)
  val opponent = (-1)*currentPlayer

  def onBoard(p: (Int,Int)): Boolean = {
    val (x,y) = p
    (x >= 0) && (x < size) && (y >= 0) && (y < size)
  }

  def getAdjacent(p: (Int,Int)): List[(Int,Int)] = {
    val (x,y) = p
    val adjacent = List((x-1,y),(x+1,y),(x,y-1),(x,y+1))
    adjacent.filter(onBoard)
  }

  def getSurrounding(p: (Int,Int)): List[(Int,Int)] = {
    val (x,y) = p
    val surrounding = List((x-1,y-1),(x-1,y),(x-1,y+1),
      (x,y-1),(x,y+1),
      (x+1,y-1),(x+1,y),(x+1,y+1))
    surrounding.filter(onBoard)
  }

  def isEye(p: (Int,Int)): Boolean = {
    val numAdj = getAdjacent(p).count(onBoard)
    val around: List[(Int,Int)] = getSurrounding(p)

    around.forall( (p) => board(p._1)(p._2) == board(around.head._1)(around.head._2)) &&
    around.size == 7 ||
    around.size == numAdj
  }

  def placeStone(x: Int, y: Int): Option[NaiveBoard] = {
    // Stone on spot
    if (board(x)(y) == 0) return None

    board(x)(y) = currentPlayer



  }


}









