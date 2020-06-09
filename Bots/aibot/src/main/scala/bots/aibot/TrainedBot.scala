package bots.aibot

import java.io.File

import bots.framework.Direction45
import org.apache.commons.io.FilenameUtils
import org.datavec.api.records.reader.impl.csv.CSVRecordReader
import org.datavec.api.records.reader.impl.transform.TransformProcessRecordReader
import org.datavec.api.split.FileSplit
import org.datavec.api.transform.TransformProcess
import org.datavec.api.transform.schema.Schema
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator
import org.deeplearning4j.earlystopping.saver.LocalFileModelSaver
import org.deeplearning4j.earlystopping.scorecalc.DataSetLossCalculator
import org.deeplearning4j.earlystopping.termination.MaxEpochsTerminationCondition
import org.deeplearning4j.earlystopping.trainer.EarlyStoppingTrainer
import org.deeplearning4j.earlystopping.{EarlyStoppingConfiguration, EarlyStoppingResult}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.nd4j.common.io.ClassPathResource
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator

class TrainedBot {

  def init() = {
    val sequential = DRLModels.createCustomNetwork(Direction45.ALL.size, DeepLearningBotViewAnalyzer.obstacleCodes.size)
    trainModelWithEarlyStopping(sequential.getNetwork, 100, 1000)
  }

  private def trainModelWithEarlyStopping(network: MultiLayerNetwork, epochs: Int, batchSize: Int): MultiLayerNetwork = {
    val tempDir: String = System.getProperty("java.io.tmpdir")
    val modelDirName: String = FilenameUtils.concat(tempDir, "ScalatronBotEarlyStopping/")
    val modelDir: File = new File(modelDirName)
    modelDir.mkdir()
    println(s"Model Dir: ${modelDir.getPath}")

    val saver = new LocalFileModelSaver(modelDirName)

    val trainData = createDataIterator("/traindata/u1.base", batchSize)
    val testData = createDataIterator("/traindata/u1.test", batchSize)

    val stoppingConf = new EarlyStoppingConfiguration.Builder()
      .epochTerminationConditions(new MaxEpochsTerminationCondition(epochs))
      //.iterationTerminationConditions(new MaxTimeIterationTerminationCondition(5, TimeUnit.MINUTES))
      .scoreCalculator(new DataSetLossCalculator(testData, true))
      .evaluateEveryNEpochs(10)
      .modelSaver(saver)
      .build()

    val trainer = new EarlyStoppingTrainer(stoppingConf, network, trainData)
    val result: EarlyStoppingResult[MultiLayerNetwork] = trainer.fit()

    result.getBestModel
  }

  private def createDataIterator(filePath: String, batchSize: Int): DataSetIterator = {
    val csvReader = new CSVRecordReader(';')
    val schema = new Schema.Builder()
      .addColumnsString("inputComamnds", "outputCommands")
      .build()
    val transformProcess = new TransformProcess.Builder(schema)
      .build()
    val transformReader = new TransformProcessRecordReader(csvReader, transformProcess)
    transformReader.initialize(new FileSplit(new ClassPathResource(filePath).getFile))
    new RecordReaderDataSetIterator.Builder(transformReader, batchSize)
      .build()
  }
}
