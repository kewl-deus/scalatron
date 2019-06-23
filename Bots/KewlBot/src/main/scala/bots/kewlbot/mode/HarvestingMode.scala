package bots.kewlbot.mode

import bots.kewlbot.cmd.{Move, Say, Status}
import bots.kewlbot.util.{Pointer, ViewSpace, XY}
import bots.kewlbot.kewlbot._

import scala.collection.immutable.List

class HarvestingMode extends MasterBot {

  /**
   * Indicates how much the distance to a Zugar is more expensive than to a Fluppet at same distance
   */
  val ZugarWeightFactor = 1.1

  def react(view: ViewSpace, params: CommandParameters) = {

    //food coords and distances
    val foodResources = List(Fluppet, Zugar).map(view.offsetToNearest).flatten.map(f => Pointer(f, f.length.toInt))

    foodResources.size match {
      case 0 => {
        val newDirection = XY.randomDirection
        ModeTransition(Say("area is harvested") | Move(newDirection), MoveMode(31, newDirection, StrategyMode))
      }
      case 1 => {
        log("looking for food: " + foodResources.head.destination)
        ModeTransition(move(foodResources.head.direction, view), StrategyMode)
      } //| Status("harvesting food")
      case 2 => {
        val fluppetDistanceWeight = foodResources.head.distance
        val zugarDistanceWeight = foodResources.last.distance * ZugarWeightFactor
        val nearestFood = fluppetDistanceWeight <= zugarDistanceWeight match {
          case true => foodResources.head
          case _ => foodResources.last
        }
        log("looking for food: " + nearestFood.destination)
        ModeTransition(move(nearestFood.direction, view), StrategyMode) //| Status("harvesting food")
      }
      case _ => {
        log("Harvesting anomaly!")
        ModeTransition(Status("Harvesting anomaly!"), StrategyMode)
      }
    }

  }

  def calculateRouteToFood(view: ViewSpace): Pointer = {
    //food coords and distances
    val foodResources = List(Fluppet, Zugar).map(view.offsetToNearest).flatten.map(f => Pointer(f, f.length.toInt))

    foodResources.size match {
      case 0 => Pointer(XY.randomDirection, 31)
      case 1 => foodResources.head
      case 2 => {
        val fluppetDistanceWeight = foodResources.head.distance
        val zugarDistanceWeight = foodResources.last.distance * ZugarWeightFactor
        fluppetDistanceWeight <= zugarDistanceWeight match {
          case true => foodResources.head
          case _ => foodResources.last
        }
      }
      case _ => Pointer(XY.randomDirection, 31)
    }
  }

}
