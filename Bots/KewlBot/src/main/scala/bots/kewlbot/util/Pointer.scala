package bots.kewlbot.util

case class Pointer(destination: XY, distance: Int) {

  def direction = destination.signum
}
