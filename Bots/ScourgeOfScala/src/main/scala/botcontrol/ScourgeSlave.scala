package botcontrol

import framework.XY
import routing.UnreachableDestinationException


class ScourgeSlave(override val name: String, val startEnergy: Int, var route: Seq[XY]) extends SlaveBotControl(name) {

  val feedThreshold = 2000
  val annihilationTimer = 500

  def energy = if (hasBot) bot.energy else startEnergy

  def welcomeMessage = if (startEnergy == bot.energy) bot.status("For the sworm!")

  override protected def run = {

    welcomeMessage

    //annihilation
    if (bot.time % annihilationTimer == 0 && bot.view.count(EnemyBot, EnemySlave) > 0) {
      bot.say("Annihilate!")
      bot.explode(10)
    } else {
      normalRun
    }
  }

  def normalRun = {
    if (timeLeft < 80) {
      if (timeLeft < 5 && distanceToMaster > 9) {
        bot.say("Blast!")
        bot.explode(9)
      } else {
        route = homeRoute
        bot.move(route.head)
        route = route.tail
      }
    } else {
      if (shouldAttack) {
        findNearestEnemy(bot.position, bot.view) match {
          case Some(enemyOffset) => attack(enemyOffset)
          case _ => harvest
        }
      } else {
        harvest
      }
    }
  }


  def shouldAttack = {
    val cells = surroundingArea(3).map(xy => bot.view.cellAtRelPos(xy))
    val attackEnemyBot = cells.contains(EnemyBot) && energy < 1500
    val attackEnemySlave = cells.contains(EnemySlave) && energy < 400
    val attackSnorgs = cells.count(c => c == Snorg) > 3 && energy < 300
    val noFriendlyFire = friendsAround < 4 && distanceToMaster > 5
    (attackEnemyBot || attackEnemySlave || attackSnorgs) && noFriendlyFire
  }

  def attack(enemyOffset: XY) = {
    bot.explode(enemyOffset.length.toInt.max(4))
    bot.say("aaaaaaargh!")
  }

  def friendsAround = bot.view.count(SlaveBot)

  def harvest = {
    val foods = findFoodResources(bot.position, bot.view)

    if (foods.size > friendsAround + 4) {
      val randomFood = foods(rnd.nextInt(foods.size))
      val slaveRoute = findRoute(bot.position, randomFood, bot.view)
      val slave = new ScourgeSlave("slave" + System.currentTimeMillis(), 100, slaveRoute.tail)
      Slavery.enslave(slave)
      bot.spawn(slaveRoute.head, slave.name, slave.startEnergy)
    }

    if (route.isEmpty || collisionPrediction(bot.position, route.head, bot.view)) {
      log("slave: routing...")
      try {
        bot.status("feed: " + bot.energy)

        route = energy > feedThreshold match {
          case true => homeRoute
          case _ => foods.size match {
            case 0 => homeRoute
            case 1 => findRoute(bot.position, foods.head, bot.view)
            case _ => {
              if (friendsAround > 1) {
                val randomFood = foods(rnd.nextInt(foods.size))
                findRoute(bot.position, randomFood, bot.view)
              } else {
                findNearestFood(bot.position, bot.view) match {
                  case Some(f) => findRoute(bot.position, f, bot.view)
                  case _ => homeRoute
                }
              }
            }
          }
        }
      } catch {
        case ude: UnreachableDestinationException => {
          log(ude)
          route = List(XY.randomDirection)
        }
      }
    }



    log("slave: route = " + route)
    bot.move(route.head)

    route = route.tail
  }

  def homeRoute: Seq[XY] = {
    try {
      bot.status("return: " + bot.energy)

      def bounded(x: Int) = if (x.abs > bot.view.size) {
        x.signum * (bot.view.size - 1)
      } else {
        x
      }

      val offsetToMaster = bot.position + bot.offsetToMaster
      val boundedMasterPos = XY(bounded(offsetToMaster.x), bounded(offsetToMaster.y))
      log("slave: homeRoute (V = virtual master)")
      log("masteroffset = " + bot.offsetToMaster)
      log("calculated master pos = " + boundedMasterPos)
      val route = findRoute(bot.position, boundedMasterPos, bot.view)
      //dont take the full route, just make same steps
      route.take(bot.view.size / 2)
      route
    } catch {
      case ex: Throwable => List(XY.randomDirection)
    }
  }

  def distanceToMaster = bot.offsetToMaster.length

  /**
   * checks if master gets out of sight
   * @return
   */
  def shouldReturnHome = bot.energy > 900 || distanceToMaster + 2 > bot.view.centerToCornerDistance
}