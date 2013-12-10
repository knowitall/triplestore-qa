package edu.knowitall.lm

import java.net.URL
import java.net.URI
import java.net.URLEncoder
import scalaj.http.Http
import scala.io.Source
import scalaj.http.HttpOptions
import com.typesafe.config.ConfigFactory
import edu.knowitall.util.MathUtils
import org.slf4j.LoggerFactory

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
  val logger = LoggerFactory.getLogger(this.getClass)
  val root = s"${url}/score"
  val retries = KenLmServer.retries
  
  override def query(s: String): Double = query(s, 1)
  
  private def query(s: String, attempt: Int = 1): Double = {
    if (attempt > retries) {
      throw new IllegalStateException("Unable to query KenLM for '$s'")
    } else {
      try {
        logger.debug(s"Querying for one string (attempt $attempt/$retries): $s")
        scaleValue(Http(root).params("q" -> s).asString.toDouble)
      } catch {
        case e: Throwable => {
          query(s, attempt + 1)
        }
      }
    }
  }

  def queryBatch(s: Iterable[String]) = {
    logger.debug(s"Querying for ${s.size} strings")
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
  val retries = conf.getInt("lm.retries")
  val batchSize = conf.getInt("lm.batchSize");
  val minValue = conf.getDouble("lm.minValue")
  val maxValue = conf.getDouble("lm.maxValue")
  val scale = conf.getBoolean("lm.scale")
}
