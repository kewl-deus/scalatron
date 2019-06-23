package botcontrol

object Slavery {
  private var barracks: Map[String, SlaveBotControl] = Map()

  def clear = barracks = Map()

  def enslave(slave: SlaveBotControl) = barracks = barracks.updated(slave.name, slave)

  def getSlave(name: String) = barracks.get(name)

  def slaveCount = barracks.size

  def slaves = barracks.values
}