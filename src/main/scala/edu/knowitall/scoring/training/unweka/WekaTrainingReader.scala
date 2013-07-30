package edu.knowitall.scoring.training.unweka

import edu.knowitall.scoring.training.TrainingDataReader
import edu.knowitall.scoring.features.AnswerGroupFeatures
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.tool.conf.Labelled
import weka.core.Instance
import weka.core.Instances
import weka.core.FastVector
import weka.core.Attribute

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
  
  private val attrInfo = {
    val fastVect = new FastVector()
    val featureAttrs = featureSet.featureNames.map(name => new Attribute(name))
    val attributes = labelAttr +: featureAttrs
    attributes foreach fastVect.addElement
    fastVect
  }
  
  def toInstance(instances: Instances)(datum: Labelled[AnswerGroup]): Instance = {
    val classValue = if (datum.label) 1.0 else 0.0
    val attrWeights = classValue +: featureSet.vectorize(datum.item)
    val inst = new Instance(1.0, attrWeights.toArray)
    inst.setDataset(instances)
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