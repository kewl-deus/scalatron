package bots.intercept

import bots.reference.CommandParser

abstract class BotControlInterceptor(controlFunction: String => String, botGeneration: Int) {

  def create = (input: String) => {

    val (opcode, params) = CommandParser(input)
    val generation = params.get("generation").map(_.toInt).getOrElse(0)

    val output = controlFunction(input)
    if( generation <= this.botGeneration ) {
      intercept(input, output)
    } else {
      output
    }
  }

  def intercept(input: String, output: String): String
}

class DebugPrintInterceptor(controlFunction: String => String, botGeneration: Int) extends BotControlInterceptor(controlFunction, botGeneration){

  override def intercept(input: String, output: String): String = {
    println(s"$input -> $output")
    output
  }
}

