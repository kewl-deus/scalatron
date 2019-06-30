package bots.aibot

import bots.framework.XY
import bots.framework.CellCodes.nameCellCode

/**
  * Cell on the board at specific position
  * @param code character defining the content
  * @param position coordinates on board
  */
case class Cell(code: Char, position: XY) {

  override def toString: String = {
    val cellName = nameCellCode(code)
    s"Cell($cellName, $position)"
  }
}

