package edu.knowitall.scoring.training.unweka

import edu.knowitall.scoring.training.TrainingDataReader
import edu.knowitall.scoring.features.AnswerGroupFeatures
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.tool.conf.Labelled
import weka.core.Instance
import weka.core.Instances
import weka.core.FastVector
import weka.core.Attribute
import scala.collection.JavaConverters._

object WekaTrainingReader {

  private def rawTrainingData = TrainingDataReader.defaultTraining
  private val featureSet = AnswerGroupFeatures
  
  private val labelValues = {
    val vect = new FastVector()
    vect.addElement("false")
    vect.addElement("true")
    vect
  }
  
  private val labelAttr = new Attribute("label", labelValues)
  
  private val attrs = {
    val featureAttrs = featureSet.featureNames.map(name => new Attribute(name))
    labelAttr +: featureAttrs
  }
  
  private val attrInfo = {
    val fastVect = new FastVector()
    attrs foreach fastVect.addElement
    fastVect
  }
  
  def toInstance(instances: Instances)(datum: Labelled[AnswerGroup]): Instance = {
    val classValue = if (datum.label) 1.0 else 0.0
    val attrValues = classValue +: featureSet.vectorize(datum.item)
    val inst = new Instance(attrs.size)
    inst.setDataset(instances)
    attrs.zip(attrValues).foreach { case (attr, value) => inst.setValue(attr, value) }
    inst
  }
  
  def toInstances(training: Iterable[Labelled[AnswerGroup]])  = {
    val insts = new Instances("Default training instances", attrInfo, 2000)
    training map toInstance(insts) foreach insts.add
    insts.setClass(labelAttr)
    insts
  }
  
  def defaultInstances = toInstances(rawTrainingData)
}