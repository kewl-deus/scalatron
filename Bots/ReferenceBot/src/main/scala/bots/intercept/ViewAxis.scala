package bots.intercept

import bots.reference.Direction45

/**
  * Axis in view containing cells along a direction
 *
  * @param direction45
  * @param cells
  */
case class ViewAxis(direction45: Int, cells: Seq[Cell]) {

  override def toString: String = {
    val dirName = nameDirection45(direction45)
    s"ViewAxis($dirName, $cells)"
  }



  def nameDirection45(index: Int): String = index match {
    case Direction45.Right => "Right"
    case Direction45.RightUp => "RightUp"
    case Direction45.Up => "Up"
    case Direction45.UpLeft => "UpLeft"
    case Direction45.Left => "Left"
    case Direction45.LeftDown => "LeftDown"
    case Direction45.Down => "Down"
    case Direction45.DownRight => "DownRight"
  }
}