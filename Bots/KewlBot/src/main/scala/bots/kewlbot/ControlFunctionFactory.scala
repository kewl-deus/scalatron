package bots.kewlbot

class ControlFunctionFactory {
  def create: (String) => String = new Bot().respond _
}
