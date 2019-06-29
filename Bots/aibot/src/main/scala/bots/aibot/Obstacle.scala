package bots.aibot

import bots.framework.XY

case class Obstacle(cell: Char, position: XY) {

  override def toString: String = {
    val cellName = Globals.nameCellCode(cell)
    s"Obstacle($cellName, $position)"
  }
}

case class ObstacleSuspicion(direction45: Int, obstacles: Seq[Obstacle]) {

  override def toString: String = {
    val dirName = Globals.nameDirection45(direction45)
    s"ObstacleSuspicion($dirName, $obstacles)"
  }
}