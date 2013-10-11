package edu.knowitall.paralex

import java.net.URL
import java.net.URI
import java.net.URLEncoder
import scalaj.http.Http
import scala.io.Source
import scalaj.http.HttpOptions

trait LanguageModel {
  /**
   * Queries a string, returns its log probability.
   */
  def query(s: String): Double
  /**
   * Does a batch query of a bunch of strings, returns a list of the (input,
   * log probability) pairs.
   */
  def query(s: Iterable[String]): List[(String, Double)]
}

case class KenLmServer(url: String = "http://localhost", port: Int = 8080) extends LanguageModel {
  val root = s"${url}:${port}/score"
  override def query(s: String): Double = {
    Http(root).params("q" -> s).asString.toDouble
  }
  override def query(s: Iterable[String]) = {
    val lst = s.toList
    val joined = lst.mkString("|")
    val lines = Http.post(root).
    			option(HttpOptions.connTimeout(10000)).
    			option(HttpOptions.readTimeout(10000)).
    			params("q" -> joined).
    			asString.trim.split("\n")
    lst.zip(lines).map { case (a, b) => (a, b.toDouble) }
  }
}