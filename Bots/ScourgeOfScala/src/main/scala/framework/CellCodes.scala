package framework

trait CellCodes {
  val MasterBot = 'M'
  val SlaveBot = 'S'

  val OccludedCell = '?'
  val EmptyCell = '_'
  val Wall = 'W'
  val EnemyBot = 'm'
  val EnemySlave = 's'

  /**good plant  */
  val Zugar = 'P'
  /**poisonous plant */
  val Toxifera = 'p'
  /**friendly creature (eatable) */
  val Fluppet = 'B'
  /**hostile creature */
  val Snorg = 'b'
}