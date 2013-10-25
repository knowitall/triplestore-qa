package edu.knowitall.scoring.learning

import scala.io.Source
import java.io.PrintWriter

abstract class SparseVector {
  def activeComponents: Iterable[String]
  def apply(i: String): Double
  def add(that: SparseVector): SparseVector
  def scalarMult(x: Double): SparseVector
  def dot(that: SparseVector): Double
  def subtract(that: SparseVector): SparseVector = this.add(that.scalarMult(-1.0))
  def +(that: SparseVector): SparseVector = this.add(that)
  def -(that: SparseVector): SparseVector = this.subtract(that)
  def *(x: Double): SparseVector = this.scalarMult(x)
  def *(that: SparseVector): Double = this.dot(that)
  def activeComponents(that: SparseVector): Iterable[String] = (this.activeComponents ++ that.activeComponents).toList.distinct
}

object SparseVector {
  def apply: SparseVector = SparseVectorImpl(Map())
  def apply(pairs: TraversableOnce[(String, Double)]): SparseVector = SparseVectorImpl(pairs.toMap)
  def apply(pairs: (String, Double)*): SparseVector = SparseVectorImpl(pairs.toMap)
  def fromFile(path: String): SparseVector = { 
    val lines = Source.fromFile(path, "UTF8").getLines
    val pairs = lines.map { line => line.split("\t") match {
      case Array(k, v) => (k, v.toDouble)
      case _ => throw new IllegalStateException(s"Could not parse line: '$line'")
    }}.toIterable
    SparseVector(pairs)
  }
  def toFile(vector: SparseVector, path: String) = {
    val writer = new PrintWriter(path)
    vector.activeComponents.foreach(k => writer.println(s"${k}\t${vector(k)}"))
    writer.close()
  }
  private case class SparseVectorImpl(map: Map[String, Double]) extends SparseVector {
    override def activeComponents = map.keys
    override def apply(i: String) = map.getOrElse(i, 0.0)
    override def add(that: SparseVector): SparseVector = {
      val pairs = for (k <- this.activeComponents(that)) yield (k, this(k) + that(k))
      SparseVectorImpl(pairs.toMap)
    }
    override def scalarMult(x: Double): SparseVector = {
      val pairs = for((k, v) <- map) yield (k, x * v)
      SparseVectorImpl(pairs.toMap)
    }
    override def dot(that: SparseVector): Double = {
      this.activeComponents(that).map(i => this(i) * that(i)).sum
    }
    override def toString = this.map.toString.replaceFirst("Map", "SparseVector")
  }
}