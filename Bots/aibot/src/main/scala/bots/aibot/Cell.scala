package bots.aibot

import bots.framework.XY

case class Cell(cellCode: Char, position: XY) {

  override def toString: String = {
    val cellName = Globals.nameCellCode(cellCode)
    s"Cell($cellName, $position)"
  }
}

case class CellVector(direction45: Int, cells: Seq[Cell]) {

  override def toString: String = {
    val dirName = Globals.nameDirection45(direction45)
    s"CellVector($dirName, $cells)"
  }
}