package bots.aibot.transform

import java.util

import bots.aibot.{CommandMap, DataStructureUtils, ViewAnalyzer}
import bots.framework.View
import org.datavec.api.transform.schema.Schema
import org.datavec.api.transform.transform.BaseTransform
import org.datavec.api.writable.{IntWritable, NDArrayWritable, Writable}

import scala.collection.JavaConversions._

class BotViewTransform(column: String, viewAnalyzer: ViewAnalyzer) extends BaseTransform {

  override def map(writables: util.List[Writable]): util.List[Writable] ={
    val columnIdx = getInputSchema.getIndexOfColumn(column)
    writables.zipWithIndex.map { case (w, index) => if (index == columnIdx) map(w).asInstanceOf[NDArrayWritable] else w }
  }

  override def map(input: Any): AnyRef = {
    val cmdMap = CommandMap(input.toString)
    val view = View(cmdMap("React").params("view"))
    val state = viewAnalyzer.getState(view)
    val features = DataStructureUtils.reshape(state)
    new NDArrayWritable(features).asInstanceOf[AnyRef]
  }

  override def mapSequence(sequence: Any): AnyRef = null

  override def transform(inputSchema: Schema): Schema = {

  }

  override def outputColumnName(): String = column

  override def outputColumnNames(): Array[String] = Array(column)

  override def columnNames(): Array[String] = Array(column)

  override def columnName(): String = column

  override def toString: String =  s"${getClass.getSimpleName}($column)"
}
