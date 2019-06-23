package bots.kewlbot.mode


import bots.kewlbot.cmd.{Explode, Move}
import bots.kewlbot.util.{ViewSpace, XY}
import bots.kewlbot.kewlbot._

case class Bodyguard(name: String) extends SlaveBot {


  def react(view: ViewSpace, params: CommandParameters) = {

    val nearestSnort: Option[XY] = view.offsetToNearest(Snorg)
    val snortCount = view.cells.count(c => c == Snorg)
    val snortDistance = view.center.distanceTo(nearestSnort.getOrElse(XY.INFINITY))

    if (snortCount > 2 && snortDistance < 40) {
      Bodyguard.guardOnAttack match {
        case None => Bodyguard.guardOnAttack = Some(this)
      }
    }

    Bodyguard.guardOnAttack match {
      case Some(guard) if guard == this => {
        nearestSnort match {
          case Some(snortPos) => {
            if (snortDistance < 20) {
              Explode(6)
            } else {
              Move(snortPos.signum)
            }
          }
          case None => {
            Bodyguard.guardOnAttack = None
            followMaster(params)
          }
        }
      }
      case _ => followMaster(params)
    }

  }

}


object Bodyguard {
  var guardOnAttack: Option[Bodyguard] = None
}