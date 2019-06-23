package framework

case class CellPointer(destination: XY, distance: Int) {
  def direction = destination.signum

  def toPath: Seq[XY] = (0 to distance).map(i => destination)

  override def toString = "%d -> %s".format(distance, destination)
}