package bots.intercept

import java.io.PrintWriter

import bots.reference._


class TrainDataCollector(controlFunction: String => String,
                         botGeneration: Int,
                         viewAnalyzer: ViewAnalyzer,
                         writer: TrainDataWriter) extends BotControlInterceptor(controlFunction, botGeneration) {

  private val directions = Direction45.Right.to(Direction45.DownRight)

  override def intercept(input: String, output: String): String = {
    val inputCommands = CommandMap(input)
    val outputCommands = CommandMap(output)

    if (inputCommands.contains("Welcome")){
      inputCommands("Welcome").paramAsInt("round").foreach(writer.startBlock)
    }
    if (inputCommands.contains("React") && !outputCommands.isEmpty) {
      val view = View(inputCommands("React").params("view"))
      val state = viewAnalyzer.getState(view)
      val move = XY(outputCommands("Move").params("direction"))
      val moveDirection = move.toDirection45
      val dirVector = directions.map(d => if (d == moveDirection) 1d else 0d)
      writer.write(state, dirVector)
    }
    if (inputCommands.contains("Goodbye")){
      writer.flush
    }

    output
  }


}



trait TrainDataWriter {

  def startBlock(index: Int)

  def write(features: Seq[Double], labels: Seq[Double])

  def flush
}

class CsvTrainDataWriter(writer: PrintWriter, separator: String = ";") extends TrainDataWriter {
  override def startBlock(index: Int) = {}

  override def write(features: Seq[Double], labels: Seq[Double]) = {
    val featuresCsv = features.mkString(separator)
    val labelsCsv = labels.mkString(separator)
    writer.println(featuresCsv + separator + labelsCsv)
  }

  override def flush = writer.flush
}