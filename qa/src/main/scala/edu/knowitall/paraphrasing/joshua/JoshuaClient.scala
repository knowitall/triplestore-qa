package edu.knowitall.paraphrasing.joshua
import edu.knowitall.util.StringUtils
import java.net.Socket
import java.io.DataOutputStream
import scala.io.Source
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import scala.io.BufferedSource
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintWriter
import java.io.OutputStreamWriter

case class JoshuaOutput(output: String, score: Double)

case object JoshuaOutput {
  val pattern = """^(.*)\t(.*)$""".r
  def fromString(s: String): Option[JoshuaOutput] = {
    s match {
      case pattern(string, value) => StringUtils.parseDouble(value) match {
        case Some(double) => Some(JoshuaOutput(string, double))
        case _ => None
      }
      case _ => None
    }
  }
} 

case class JoshuaClient(host: String, port: Int, maxHits: Int) {
  
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def this() = this(JoshuaClient.defaultHost, JoshuaClient.defaultPort, JoshuaClient.defaultMaxHits)

  def decode(input: String): List[JoshuaOutput] = {
    
    val socket = new Socket(host, port)
    socket.setKeepAlive(true)
    
    val out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()))
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    
    // Send an extra newline so we get an empty translation back 
    out.write(input + "\n\n")
    out.flush()

    // Read the lines until we see the empty translation, which is a line
    // starting with a tab.
    val lines = Stream.continually(in.readLine()).takeWhile(!_.startsWith("\t")).take(maxHits)
    val result = lines.flatMap(JoshuaOutput.fromString).toList
    socket.close()
    result
  }

}

case object JoshuaClient {
  val conf = ConfigFactory.load()
  val defaultHost = conf.getString("paraphrase.joshua.host")
  val defaultPort = conf.getInt("paraphrase.joshua.port")
  val defaultMaxHits = conf.getInt("paraphrase.joshua.maxHits")
}
