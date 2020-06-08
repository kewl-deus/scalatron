package bots.aibot

import bots.framework.{CommandParser, XY}


class CommandMap(commands: Map[String, Map[String, String]]) {

  def apply(opcode: String): Command = Command(opcode, commands.getOrElse(opcode, Map()))

  def isEmpty = commands.isEmpty

  def contains(opcode: String) = commands.contains(opcode)
}

object CommandMap {
  def apply(commands: String) = {
    if (commands.isEmpty){
      new CommandMap(Map())
    } else {
      new CommandMap(commands.split('|')
        .map(c => CommandParser(c))
        .toMap)
    }
  }
}

case class Command(opcode: String, params: Map[String, String]){
  def paramOrElse(key: String, fallback: String) = params.getOrElse(key, fallback)

  def paramAsInt(key: String) = params.get(key).map(_.toInt)

  def paramAsXY(key: String) = params.get(key).map(s => XY(s))
}
