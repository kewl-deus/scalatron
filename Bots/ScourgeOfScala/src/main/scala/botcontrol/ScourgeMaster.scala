package botcontrol

import routing.PongAlgorithm._
import framework.{View, XY}
import routing.{PongAlgorithm, UnreachableDestinationException}


object ScourgeMaster extends BotControl {

  var roundTime: Int = Int.MaxValue

  protected var route: Seq[XY] = Nil

  protected var lastDirection = XY.Zero

  def welcomeMessage = if (bot.time < 2) bot.say("The world is not enough")

  def timeLeft = roundTime - bot.time

  override protected def run = {

    welcomeMessage

    //spawning
    if (timeLeft > 150 && bot.time % 2 == 0 && bot.view.count(SlaveBot) < 4) {
      spawnSlave
    }

    if (route.isEmpty || collisionPrediction(bot.position, route.head, bot.view)) {
      log("routing...")
      try {
        if (Slavery.slaveCount > 20 && bot.energy > 20000) {
          bot.status("enslave")
          def wallDensity = bot.view.count(Wall, Toxifera, Snorg).toFloat / bot.view.cells.size.toFloat
          def wallsAround = surroundingArea(1).count(c => bot.view.cellAtRelPos(c) == Wall)  > 4
          if (wallDensity > 0.05 || wallsAround) {
            route = findRoute(bot.position, findRandomTarget(bot.position, bot.view), bot.view)
          } else {
            val pongDir = PongAlgorithm(bot.view, lastDirection)
            if (pongDir != lastDirection) {
              bot.say("pong")
              route = List(pongDir)
            } else {
              route = List(lastDirection)
            }
          }
        } else {
          route = calcFoodPath(bot.position, bot.view)
        }

      } catch {
        case ude: UnreachableDestinationException => {
          log(ude)
          route = List(XY.randomDirection)
        }
      }
    }

    //move master
    log("route = " + route)
    lastDirection = route.head
    bot.move(route.head)
    route = route.tail

  }

  def spawnSlave = {
    val foods = findFoodResources(bot.position, bot.view).filter(f => f.length < 21)
    log("detected %d foodresources in minibot range", foods.size)
    if (foods.size > 0) {
      log("spawning slave")
      val randomFood = foods(rnd.nextInt(foods.size))
      val slaveRoute = findRoute(bot.position, randomFood, bot.view)
      val slave = new ScourgeSlave("slave" + System.currentTimeMillis(), 100, slaveRoute.tail)
      Slavery.enslave(slave)
      bot.spawn(slaveRoute.head, slave.name, slave.startEnergy)
    }
  }

  def calcFoodPath(origin: XY, view: View) = {
    val foodOption = findNearestFood(origin, view)

    val destination = foodOption match {
      case Some(food) => {
        bot.status("harvesting")
        food
      }
      case _ => {
        bot.status("randomizing")
        findRandomTarget(origin, view)
      }
    }
    findRoute(origin, destination, view)
  }

  def findRandomTarget(origin: XY, view: View) = {
    var target = origin

    while (view.cellAtAbsPos(target) != EmptyCell && origin.distanceTo(target) < 14) {
      val rndIndex = rnd.nextInt(view.cells.length)
      target = view.absPosFromIndex(rndIndex)
    }
    target
  }

}

