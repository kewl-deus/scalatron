package bots.kewlbot.cmd

import bots.kewlbot.util.XY

import math.signum

case class Say(text: String) extends Command("Say") {
  override def toString = {
    this <<("text", text)
    super.toString
  }
}

case class Status(text: String) extends Command("Status") {
  override def toString = {
    this <<("text", text)
    super.toString
  }
}

case class Move(dx: Int, dy: Int) extends Command("Move") {

  override def toString = {
    this << ("direction", "%d:%d".format(signum(dx),signum(dy)))
    super.toString
  }
}

object Move {
  def apply(xy: XY) = new Move(xy.x, xy.y)
}

case class Spawn(dx: Int, dy: Int, name: String, energy: Int) extends Command("Spawn") {
  override def toString = {
    this << ("direction", "%d:%d".format(signum(dx),signum(dy))) <<("name", name) <<("energy", energy)
    super.toString
  }
}

case class Explode(size: Int) extends Command("Explode") {
  override def toString = {
    this <<("size", size)
    super.toString
  }
}
