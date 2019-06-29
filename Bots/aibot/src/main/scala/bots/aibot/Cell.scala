package bots.aibot

import bots.framework.XY

case class Cell(cellCode: Char, position: XY) {

  override def toString: String = {
    val cellName = Globals.nameCellCode(cellCode)
    s"Cell($cellName, $position)"
  }
}

