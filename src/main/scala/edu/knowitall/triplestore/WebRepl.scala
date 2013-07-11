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


object WebRepl extends App {

  val logger = LoggerFactory.getLogger(this.getClass) 
 
  object Plan extends unfiltered.filter.Plan {
    
    val repl = SimpleRepl()
    
    def intent = Intent {
      
      case req @ GET(Path(Seg("query" :: Nil))) => 
        runQuery(req.parameterValues("q").mkString(" "))

      case req @ GET(Path(path)) => getStatic(path)
        
    }
    
    def runQuery(query: String) = {
      val result = repl.eval(query)
      ResponseString(result) ~> Ok      
    }
    
    def getStatic(path: String) = {
      val mimeType = Mime.unapply(path).getOrElse("text/plain")
      val realPath = "web/" + { if (path.trim() == "/") "index.html" else path } 
      val stream = getClass.getClassLoader().getResourceAsStream(realPath)
        if (stream != null) {
          System.out.println(mimeType)
          ContentEncoding(mimeType) ~> ResponseBytes(IOUtils.toByteArray(stream)) ~> Ok
        } else {
          ResponseString("Not found!") ~> NotFound
        }
      }
  }
  
  Http(8080).filter(Plan).run()

}