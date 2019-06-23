package framework

import scala.collection.Seq

case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size

  def absPosFromIndex(index: Int) = XY(index % size, index / size)

  def absPosFromRelPos(relPos: XY) = relPos + center

  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))

  def relPosFromAbsPos(absPos: XY) = absPos - center

  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))

  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  private def offsetToNearestSingle(c: Char) = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if (matchingXY.isEmpty)
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      Some(nearest)
    }
  }

  def centerToCornerDistance = center.length.toInt

  def contains(c: Char): Boolean = cells.contains(c)

  def count(ch: Char*): Int = cells.count(cell => ch.contains(cell))

  def offsetToNearest(ch: Char*) = {
    val offsets = ch.distinct.map(offsetToNearestSingle(_)).flatten
    if (offsets.isEmpty){
      None
    } else {
      Some(offsets.minBy(_.length))
    }
  }
  
  def quadrant(q: Int): View = {
    val q0 = q % 4 //quadrant counting with 0-3
    val offset = q0 % 2 match {
      case 0 => 0
      case 1 => cells.length / 2
    }
    val slice = cells.slice(offset, offset + cells.length / 2)
    val qCells = slice.sliding(size / 2, size)
    View(qCells.mkString)
  }

  def set(absPos: XY, value: Char): View = new View(cells.updated(indexFromAbsPos(absPos), value).mkString)

  def set(path: Seq[XY], value: Char): View = path.foldLeft(this)((v, point) => v.set(point, value))

  override def toString = {
    new String(cells.map(x =>
      if (x < 10) " " + x else x).sliding(size, size).flatMap(l =>
      l.addString(new StringBuilder, "", "\t", "\n")).toArray)
  }
}