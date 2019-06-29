package bots.aibot

import bots.framework.{CellCodes, Direction45}

object Globals extends CellCodes {

  type State = Seq[Double]

  val Noop = ""

  val maxSteps = 15
  val obstacleCodes = List(OccludedCell, Wall, Zugar, Toxifera, Fluppet, Snorg)
  val directions = Direction45.Right.to(Direction45.DownRight)

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

  def nameCellCode(cellCode: Char): String = cellCode match {
    case MasterBot => "MasterBot"
    case SlaveBot => "SlaveBot"
    case OccludedCell => "OccludedCell"
    case EmptyCell => "EmptyCell"
    case Wall => "Wall"
    case EnemyBot => "EnemyBot"
    case EnemySlave => "EnemySlave"
    case Zugar => "Zugar"
    case Toxifera => "Toxifera"
    case Fluppet => "Fluppet"
    case Snorg => "Snorg"
    case _ => cellCode.toString
  }
}
