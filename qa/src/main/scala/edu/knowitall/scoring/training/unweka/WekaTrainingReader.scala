package edu.knowitall.scoring.training.unweka

import edu.knowitall.scoring.training.TrainingDataReader
import edu.knowitall.scoring.features.AnswerGroupFeatures
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.tool.conf.Labelled
import weka.core.DenseInstance
import weka.core.Instance
import weka.core.Instances
import weka.core.Attribute
import scala.collection.JavaConversions._
import java.util.ArrayList

object WekaTrainingReader {

  private def rawTrainingData = TrainingDataReader.defaultTraining
  private val featureSet = AnswerGroupFeatures
  
  private val classValues = List("positive", "negative")
  
  private val classAttr = new Attribute("class", classValues)
  
  private val attrs = {
    val featureAttrs = featureSet.featureNames.map(name => new Attribute(name))
    featureAttrs :+ classAttr
  }
  
  private val attrsList = {
    val list = new ArrayList[Attribute](attrs.size)
    attrs foreach list.add
    list
  }
  
  def toUnlabeledInstance(instances: Instances)(group: AnswerGroup): Instance = {
    val attrValues = featureSet.vectorize(group)
    val inst = new DenseInstance(attrValues.size)
    inst.setDataset(instances)
    attrs.zip(attrValues).foreach { case (attr, value) => inst.setValue(attr, value) }
    inst
  }
  
  def toLabeledInstance(instances: Instances)(datum: Labelled[AnswerGroup]): Instance = {
    val label = if (datum.label) 1.0 else 0.0
    val attrValues = featureSet.vectorize(datum.item) :+ label
    val inst = new DenseInstance(attrs.size)
    inst.setDataset(instances)
    attrs.zip(attrValues).foreach { case (attr, value) => inst.setValue(attr, value) }
    inst
  }
  
  def toInstances(training: Iterable[Labelled[AnswerGroup]])  = {
    val insts = new Instances("Default training instances", attrsList, 0)
    insts.setClass(classAttr)
    training map toLabeledInstance(insts) foreach insts.add
    insts
  }
  
  def defaultInstances = toInstances(rawTrainingData)
}