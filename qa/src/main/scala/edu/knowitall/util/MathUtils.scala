package edu.knowitall.util

import org.slf4j.LoggerFactory

object MathUtils {
  val logger = LoggerFactory.getLogger(this.getClass)

  def clip(x: Double, min: Double, max: Double) = Math.min(Math.max(x, min), max)
  def scale(x: Double, min: Double, max: Double) = (x - min) / (max - min)
  def clipScale(x: Double, min: Double, max: Double) = scale(clip(x, min, max), min, max)

}