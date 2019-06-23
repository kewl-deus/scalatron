package bots.kewlbot.util

case class SquareView(val cells: String) {

  lazy val len: Int = math.sqrt(cells.length).toInt

  def get(row: Int, col: Int): Char = cells(row * len + col)

  def getRow(r: Int): Seq[Char] = cells.slice(r * len, (r + 1) * len)

  def getColumn(c: Int): Seq[Char] = try {
    (0 until len).map(get(_, c))
  } catch {
    case iob: IndexOutOfBoundsException => Nil
  }

  def getLeft2RightDiagonal: Seq[Char] = (0 until len).map(i => get(i, i))

  def getRight2LeftDiagonal: Seq[Char] = (0 until len).map(i => get(i, len - i - 1))

  def contains(c: Char): Boolean = cells.contains(c)


  override def toString = {
    new String(cells.map(x =>
      if (x < 10) " " + x else x).sliding(len, len).flatMap(l =>
      l.addString(new StringBuilder, "", "\t", "\n")).toArray)
  }
}
