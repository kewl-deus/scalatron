package scalatron.main

import java.io.{File, FileWriter, PrintWriter}

import bots.intercept.CellCodes._
import bots.intercept._
import scalatron.core.EntityController
import scalatron.scalatron.impl.EntityControllerImpl


object Bots {

  val MasterBotGeneration = 0

  lazy val controllers: Seq[EntityController] = List(interceptTrainDataCsv(ReferenceBot))

  lazy val ReferenceBot = new EntityControllerImpl("ReferenceBot", new bots.reference.ControlFunctionFactory().create)

  lazy val DeepThought = new EntityControllerImpl("DeepThought", new bots.deepthought.ControlFunctionFactory().create)

  lazy val AIBot = new EntityControllerImpl("Genisys", new bots.aibot.ControlFunctionFactory().create)


  def intercept(entityController: EntityControllerImpl, interceptor: BotControlInterceptor): EntityController =
    new EntityControllerImpl(entityController.name, interceptor.create)

  def interceptDebugPrint(entityController: EntityControllerImpl): EntityController =
    intercept(entityController, new DebugPrintInterceptor(entityController.controlFunction, MasterBotGeneration))

  def interceptTrainDataCsv(entityController: EntityControllerImpl): EntityController = {
    val viewAnalyzer = new ViewAnalyzer(List(Wall, Zugar, Toxifera, Fluppet, Snorg), EnvironmentInterpreters.obstacleBitmap)

    val timestamp = System.currentTimeMillis
    val file = new File(System.getProperty("java.io.tmpdir"), s"scalatron-traindata-$timestamp.csv")
    println(s"Saving traindata to $file")

    val writer = new CsvTrainDataWriter(new PrintWriter(new FileWriter(file)))
    val interceptor = new TrainDataCollector(entityController.controlFunction, MasterBotGeneration, viewAnalyzer, writer)

    intercept(entityController, interceptor)
  }

}
