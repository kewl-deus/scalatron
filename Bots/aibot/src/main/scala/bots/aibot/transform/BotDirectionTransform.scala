package bots.aibot.transform

import java.util

import bots.aibot.CommandMap
import bots.framework.Direction45
import org.datavec.api.transform.metadata.IntegerMetaData
import org.datavec.api.transform.schema.Schema
import org.datavec.api.transform.transform.BaseTransform
import org.datavec.api.writable.{IntWritable, Writable}
import scala.collection.JavaConversions._

class BotDirectionTransform(column: String) extends BaseTransform {

  override def map(writables: util.List[Writable]): util.List[Writable] = {
    val columnIdx = getInputSchema.getIndexOfColumn(column)
    writables.zipWithIndex.map { case (w, index) => if (index == columnIdx) map(w).asInstanceOf[IntWritable] else w }
  }


  override def map(input: Any): AnyRef = {
    val commandMap = CommandMap(input.toString)
    val dir = commandMap("Move").paramAsXY("direction").map(_.toDirection45).getOrElse(0)
    new IntWritable(dir).asInstanceOf[AnyRef]
  }

  override def mapSequence(sequence: Any): AnyRef = null

  override def transform(schema: Schema): Schema = {
    val columnIdx = schema.getIndexOfColumn(column)


    val newMeta = schema.getColumnNamesIndex.toMap.map { case (name, index) => if (columnIdx == index) {
      new IntegerMetaData(name, Direction45.Right, Direction45.DownRight)
    } else {
      schema.getMetaData(name)
    }
    }

    schema.newSchema(newMeta.toList)
  }

  override def outputColumnName(): String = column

  override def outputColumnNames(): Array[String] = Array(column)

  override def columnNames(): Array[String] = Array(column)

  override def columnName(): String = column

  override def toString: String =  s"${getClass.getSimpleName}($column)"
}
