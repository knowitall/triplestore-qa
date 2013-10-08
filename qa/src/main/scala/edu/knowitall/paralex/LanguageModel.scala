package edu.knowitall.paralex

import java.net.URL
import java.net.URI
import java.net.URLEncoder
import scalaj.http.Http
import scala.io.Source

trait LanguageModel {
  
  def query(s: String): Double
  
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
    val lines = Http.post(root).params("q" -> joined).asString.trim.split("\n")
    lst.zip(lines).map { case (a, b) => (a, b.toDouble) }
  }
}
case object KenLmServer {
  def escape(s: String) = URLEncoder.encode(s, "UTF8").replace("+", "%20")
}

case object LMTest extends App {
  val lm = KenLmServer("http://localhost", 8089)
  val lines = Source.fromInputStream(System.in, "UTF8").getLines.toList
  lm.query(lines).foreach { case (a,b) => println(s"$b\t$a") }
}