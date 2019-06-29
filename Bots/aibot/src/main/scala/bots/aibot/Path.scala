package bots.aibot

import bots.framework.{View, XY}

class Path(val positions: Seq[XY]) {
  def cells(view: View) = positions.map(pos => view(pos))

  def apply(view: View) = cells(view)

  def apply(index: Int): XY = positions(index)

  override def toString: String = {
    val csvPositions = positions.mkString(",")
    s"Path($csvPositions)"
  }
}


object Path {

  def apply(positions: Seq[XY]): Path = new Path(positions)

  def apply(position: XY, steps: Int) = new Path(Stream.iterate(position, steps)(streamPos => streamPos + position))

}