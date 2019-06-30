package bots.aibot

import bots.framework.Direction45.nameDirection45

/**
  * Axis in view containing cells along a direction
  * @param direction45
  * @param cells
  */
case class ViewAxis(direction45: Int, cells: Seq[Cell]) {

  override def toString: String = {
    val dirName = nameDirection45(direction45)
    s"ViewAxis($dirName, $cells)"
  }
}