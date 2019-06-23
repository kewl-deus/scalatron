package bots.kewlbot.util

import scala.util.Random

case class XY(x: Int, y: Int) {
  def +(pos: XY) = XY(x + pos.x, y + pos.y)

  def -(pos: XY) = XY(x - pos.x, y - pos.y)

  def distanceTo(pos: XY): Double = (this - pos).length // Phythagorean

  def length: Double = math.sqrt(x * x + y * y) // Phythagorean

  def signum = XY(x.signum, y.signum)

  override def toString = "XY(%d,%d)".format(x,y)
}


object XY {
  
  private val rnd = new Random()
  
  val INFINITY = new XY(Int.MaxValue, Int.MaxValue)
  
  def randomDirection = {
    var xy = XY(0,0)
    while (xy == XY(0,0)){
      xy = XY(rnd.nextInt(3) - 1, rnd.nextInt(3) - 1)
    }
    xy
  }

}