package edu.knowitall.util

object StringUtils {
  
  def parseDouble(s: String): Option[Double] = try { Some(s.toDouble) } catch { case e:Throwable => None }

}