package botcontrol

import framework.XY
import routing.PathCompressor


class TracingMiniBot(override val name: String, val feedThreshold: Int) extends SlaveBotControl(name) {

  var breadcrumbs: List[XY] = List()

  protected def run = {
    //harvest food and track your own path
    //when finished, trace back own path than trace back to master by its breadcrumbs

    val spawnOffset = bot.inputAsXYOrElse("spawnOffset", XY.Zero)
    if (spawnOffset != XY.Zero) {
      bot.set(("spawnOffset", XY.Zero))
      breadcrumbs = spawnOffset.signum.negate :: breadcrumbs
    }

    var homecoming = bot.inputOrElse("homecoming", "false").toBoolean
    homecoming = homecoming || bot.energy > feedThreshold

    val direction = homecoming match {
      case true => {
        if (distanceToMaster < 3){
          bot.offsetToMaster.signum
        } else {
          nextBreadcrumb
        }
      }
      case _ => {
        bot.status("feed: " + bot.energy)
        val direction = bot.view.offsetToNearest(Fluppet, Zugar) match {
          case Some(food) => {
            val dir = food.signum
            breadcrumbs = dir.negate :: breadcrumbs
            dir
          }
          case _ => {
            homecoming = true
            nextBreadcrumb
          }
        }

        direction
      }
    }
    bot.move(direction)
    bot.set(("homecoming", homecoming))
  }


  private def nextBreadcrumb = breadcrumbs.isEmpty match {
    case true => {
      bot.status("follow: " + bot.energy)
      val masterBreadcrumbs = MasterBotControl.breadcrumbs

      var breadcrumbIndex = bot.inputAsIntOrElse("breadcrumbIndex", Int.MaxValue)
      //printf("%s following with breadcrumbIndex = %d - masterBC.size = %d \n", name, breadcrumbIndex, masterBreadcrumbs.size)

      //try way reduction by skipping loops
      val path = masterBreadcrumbs.takeRight(masterBreadcrumbs.size - breadcrumbIndex + 1)
      val skipIndex = PathCompressor.findSkipIndex(path)
      breadcrumbIndex = breadcrumbIndex + (skipIndex - 1).max(0)

      breadcrumbIndex = breadcrumbIndex.min(masterBreadcrumbs.size - 1)

      val breadcrumb = masterBreadcrumbs(breadcrumbIndex)
      bot.set(("breadcrumbIndex", breadcrumbIndex + 1))
      breadcrumb
    }
    case false => {
      bot.status("backtrace: " + bot.energy)
      val breadcrumb = breadcrumbs.head
      breadcrumbs = breadcrumbs.tail
      breadcrumb
    }
  }
}
