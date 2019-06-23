package bots.kewlbot

import bots.kewlbot.cmd.BotInstruction

package object kewlbot {

  type CommandParameters = Map[String, String]

  val MasterBot = 'M'
  val SlaveBot = 'S'

  val OccludedCell = '?'
  val EmptyCell = '_'
  val Wall = 'W'
  val EnemyBot = 'm'
  val EnemySlave = 's'
  val Zugar = 'P' //good plant
  val Toxifera = 'p' //poisonous plant
  val Fluppet = 'B' //good creature (eatable)
  val Snorg = 'b' //bad creature (hostile)

  implicit def commandToString(cmd: BotInstruction): String = cmd.toString
}
