package bots.framework

trait CellCodes {
  val MasterBot = 'M'
  val SlaveBot = 'S'

  val OccludedCell = '?'
  val EmptyCell = '_'
  val Wall = 'W'
  val EnemyBot = 'm'
  val EnemySlave = 's'

  /** good plant  */
  val Zugar = 'P'

  /** poisonous plant */
  val Toxifera = 'p'

  /** friendly creature (eatable) */
  val Fluppet = 'B'

  /** hostile creature */
  val Snorg = 'b'

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