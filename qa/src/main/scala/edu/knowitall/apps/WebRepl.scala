package edu.knowitall.apps
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
import unfiltered.request.HttpRequest
import scala.collection.immutable.ListMap
import edu.knowitall.paraphrasing.template.TemplateParaphraser


object WebRepl extends App {

  val logger = LoggerFactory.getLogger(this.getClass) 
   
  /**
   * This object defines the web server behavior.
   */
  object Plan extends unfiltered.filter.Plan {
    
    def intent = Intent {
      
      /**
       * Map the URL /query to the runQuery function.
       */
      case req @ GET(Path(Seg("query" :: Nil))) => 
        runQuery(req.parameterValues("q").mkString(" "), getConfig(req))
        
      case req @ GET(Path(Seg("listComponents" :: Nil))) =>
        listComponents()
        
      case req @ GET(Path(Seg("paraphrase" :: Nil))) =>
        paraphrase(req.parameterValues("q").mkString(" "))

      /**
       * Map all other URL paths to get the static content stored as a 
       * resource on the classpath.
       */
      case req @ GET(Path(path)) => getStatic(path)
        
    }
    
    val paraphraser = new TemplateParaphraser()
    def paraphrase(query: String) = {
      val results = paraphraser.paraphraseToStrings(query)
      val resultJson = JsonSerialization.serialize(results)
      ContentEncoding("application/json") ~> ResponseString(resultJson) ~> Ok
    }
    
    def getConfig(req: HttpRequest[_]): QAConfig = {
      var config = new QAConfig()
      config = getParam(req, "paraphraser") match {
        case Some(s) => config.copy(paraphraser = s)
        case _ => config
      }
      config = getParam(req, "parser") match {
        case Some(s) => config.copy(parser = s)
        case _ => config
      }
      config = getParam(req, "executor") match {
        case Some(s) => config.copy(executor = s)
        case _ => config
      }
      config = getParam(req, "grouper") match {
        case Some(s) => config.copy(grouper = s)
        case _ => config
      }
      config = getParam(req, "scorer") match {
        case Some(s) => config.copy(scorer = s)
        case _ => config
      }
      config
    }
    
    def getParam(req: HttpRequest[_], name: String): Option[String] =
      req.parameterValues(name).mkString(" ") match {
        case s: String if s.trim() != "" => Some(s)
        case _ => None
      }
    
    def runQuery(query: String, config: QAConfig) = {
      logger.info(s"Got query '$query'")
      val qa = QASystem.getInstance(config) match {
        case Some(x) => x
        case None => throw new IllegalStateException("could not get qa")
      }
      val result = qa.answer(query)
      val resultJson = JsonSerialization.serializeAnswers(result)
      logger.info(s"Finished computing results for '$query'")
      ContentEncoding("application/json") ~> ResponseString(resultJson) ~> Ok
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
    
    def listComponents() = {
      val components = ListMap(
    		  		   "paraphraser" -> Map("name" -> "Paraphraser", "options" -> Components.paraphrasers.keys.toList),
    		  		   "parser" -> Map("name" -> "Question Parsing", "options" -> Components.parsers.keys.toList),
                       "executor" -> Map("name" -> "Query Execution", "options" -> Components.executors.keys.toList),
                       "grouper" -> Map("name" -> "Answer Grouping", "options" -> Components.groupers.keys.toList.sorted),
                       "scorer" -> Map("name" -> "Answer Scoring", "options" -> Components.scorers.keys.toList))
      val result = Map("components" -> components, "defaults" -> Components.defaults) 
      val resultJson = JsonSerialization.serialize(result)
      ContentEncoding("application/json") ~> ResponseString(resultJson) ~> Ok
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