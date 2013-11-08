package edu.knowitall.lm

import java.net.URL
import java.net.URI
import java.net.URLEncoder
import scalaj.http.Http
import scala.io.Source
import scalaj.http.HttpOptions
import com.typesafe.config.ConfigFactory
import edu.knowitall.util.MathUtils

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

case class KenLmServer(url: String, timeOut: Int, scale: Boolean = KenLmServer.scale) extends LanguageModel {
  def this() = this(KenLmServer.defaultUrl, KenLmServer.defaultTimeout)
  val root = s"${url}/score"
  override def query(s: String): Double = {
    Http(root).params("q" -> s).asString.toDouble
  }
  def queryBatch(s: Iterable[String]) = {
    val lst = s.toList
    val joined = lst.mkString("|")
    val lines = Http.post(root).
    			option(HttpOptions.connTimeout(timeOut)).
    			option(HttpOptions.readTimeout(timeOut)).
    			params("q" -> joined).
    			asString.trim.split("\n")
    lst.zip(lines).map { case (a, b) => (a, scaleValue(b.toDouble)) }
  }
  override def query(s: Iterable[String]) = {
    val groups = s.grouped(KenLmServer.batchSize)
    groups.flatMap(queryBatch).toList
  }
  private def scaleValue(x: Double): Double = 
    if (scale) MathUtils.clipScale(x, KenLmServer.minValue, KenLmServer.maxValue)
    else x
}

case object KenLmServer {
  val conf = ConfigFactory.load()
  val defaultUrl = conf.getString("lm.url")
  val defaultTimeout = conf.getInt("lm.timeout")
  val batchSize = conf.getInt("lm.batchSize");
  val minValue = conf.getDouble("lm.minValue")
  val maxValue = conf.getDouble("lm.maxValue")
  val scale = conf.getBoolean("lm.scale")
}
