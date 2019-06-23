package routing

import framework.{View, XY}

object PongAlgorithm {

  def apply(view: View, currentDirection: XY = XY.randomDirection): XY = {
    val dir = ensureDiagonalDirection(currentDirection)
    nextDirection(view, dir, 2)
  }

  private def ensureDiagonalDirection(direction: XY) = {
    import XY._
    val diagonalDirections = List(DownRight, UpLeft, LeftDown, RightUp)

    var dir = direction

    if (!diagonalDirections.contains(dir)) {
      dir = diagonalDirections(new util.Random().nextInt(diagonalDirections.size))
    }
    dir
  }

  private def nextDirection(view: View, currentDirection: XY, inc: Int): XY = {
    val incX = -currentDirection.x * inc
    val incY = -currentDirection.y * inc

    for (x <- currentDirection.x to -currentDirection.x by incX) {
      for (y <- currentDirection.y to -currentDirection.y by incY) {
        val cell = view.cellAtRelPos(XY(x, y))
        if ("WspbSm".contains(cell) == false) {
          return XY(x, y)
        }
      }
    }
    XY.DownRight
  }

}
