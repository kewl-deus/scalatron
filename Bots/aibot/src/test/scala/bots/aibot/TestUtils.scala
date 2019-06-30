package bots.aibot

import bots.framework.View

object TestUtils {

  def viewFromMultiline(multilineString: String): View = View(multilineString.replaceAll("\\s", ""))
}
