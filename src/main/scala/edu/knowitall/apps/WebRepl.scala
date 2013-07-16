package edu.knowitall.triplestore
import org.slf4j.LoggerFactory
import unfiltered.filter.Intent
import unfiltered.response.ResponseString
import unfiltered.response.ResponseBytes
import unfiltered.response.ContentType
import unfiltered.response.Ok
import unfiltered.jetty.Http
import unfiltered.request.GET
import unfiltered.request.Path
import unfiltered.request.Seg
import unfiltered.response.NotFound
import unfiltered.request.Mime
import java.io.File
import unfiltered.response.ContentEncoding
import org.apache.commons.io.IOUtils
import edu.knowitall.apps.SimpleRepl


object WebRepl extends App {

  val logger = LoggerFactory.getLogger(this.getClass) 
   
  /**
   * This object defines the web server behavior.
   */
  object Plan extends unfiltered.filter.Plan {
    
    val repl = SimpleRepl(hits = 500)
    
    def intent = Intent {
      
      /**
       * Map the URL /query to the runQuery function.
       */
      case req @ GET(Path(Seg("query" :: Nil))) => 
        runQuery(req.parameterValues("q").mkString(" "))

      /**
       * Map all other URL paths to get the static content stored as a 
       * resource on the classpath.
       */
      case req @ GET(Path(path)) => getStatic(path)
        
    }
    
    def runQuery(query: String) = {
      logger.info(s"Got query '$query'")
      val result = repl.eval(query)
      logger.info(s"Finished computing results for '$query'")
      ResponseString(result) ~> Ok      
    }
    
    def getStatic(path: String) = {
      val mimeType = Mime.unapply(path).getOrElse("text/plain")
      val realPath = "/edu/knowitall/triplestore/web" + { if (path.trim() == "/") "/index.html" else path }
      val stream = getClass.getResourceAsStream(realPath)
        if (stream != null) {
          ContentEncoding(mimeType) ~> ResponseBytes(IOUtils.toByteArray(stream)) ~> Ok
        } else {
          ResponseString("Not found!") ~> NotFound
        }
      }
  }
  
  /**
   * Command line parameter parsing stuff.
   */
  case class Config(port: Int = 8080)
      
  val argParser = new scopt.OptionParser[Config]("scopt") {
    help("help") text("Starts a REPL web interface for querying the triplestore.")
    opt[Int]('p', "port") action { (x, c) => c.copy(port = x)} text("webserver port")
  }
  
  argParser.parse(args, Config()) map { config =>
    val port = config.port
    Http(port).filter(Plan).run()
    logger.info(s"Starting webserver at http://localhost:$port")
  }
}