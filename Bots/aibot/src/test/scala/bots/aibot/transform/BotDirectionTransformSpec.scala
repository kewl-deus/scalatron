package bots.aibot.transform

import org.datavec.api.records.reader.impl.csv.CSVRecordReader
import org.datavec.api.records.reader.impl.transform.TransformProcessRecordReader
import org.datavec.api.split.StringSplit
import org.datavec.api.transform.schema.Schema
import org.datavec.api.transform.{ColumnType, TransformProcess}
import org.datavec.api.writable.IntWritable
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator
import org.specs2.mutable.Specification

class BotDirectionTransformSpec extends Specification {

  "Move command" should {
    "be transformed into Integer" in {

      val schema = new Schema.Builder()
        .addColumnInteger("simpleNumber")
        .addColumnsString("commands")
        .build()

      val transformProcess = new TransformProcess.Builder(schema)
        .transform(new BotDirectionTransform("commands"))
        .build()


      val csvReader = new CSVRecordReader(';')
      csvReader.getLabels
      val transformReader = new TransformProcessRecordReader(csvReader, transformProcess)
      transformReader.initialize(new StringSplit("42;Move(direction=-1:-1)|Set(lastDirection=1)"))
      val dataSetIterator = new RecordReaderDataSetIterator.Builder(transformReader, 1)
        .classification(1, 8)
        .build()

      val dataSet = dataSetIterator.next()
      dataSet.asList().size() mustEqual 1

    }
  }
}
