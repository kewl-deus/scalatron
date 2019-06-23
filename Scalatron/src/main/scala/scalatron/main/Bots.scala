package scalatron.main

import scalatron.core.EntityController
import scalatron.scalatron.impl.EntityControllerImpl

object Bots {

  lazy val controllers: Seq[EntityController] = List(AIBot)

  lazy val ReferenceBot = new EntityControllerImpl("ReferenceBot", new bots.reference.ControlFunctionFactory().create)

  lazy val DeepThought = new EntityControllerImpl("DeepThought", new bots.deepthought.ControlFunctionFactory().create)

  lazy val AIBot = new EntityControllerImpl("aibot", new bots.aibot.ControlFunctionFactory().create)

}
