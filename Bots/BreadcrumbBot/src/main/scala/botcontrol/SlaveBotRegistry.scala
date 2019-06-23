package botcontrol

object SlaveBotRegistry {
  private var barracks: Map[String, SlaveBotControl] = Map()

  def clear = barracks = Map()

  def put(slave: SlaveBotControl) = barracks = barracks.updated(slave.name, slave)

  def get(name: String) = barracks.get(name)

  def slaveCount = barracks.size

  def slaves = barracks.values
}