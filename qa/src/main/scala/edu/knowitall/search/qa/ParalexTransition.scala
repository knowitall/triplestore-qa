package edu.knowitall.search.qa

import edu.knowitall.search.Transition
import edu.knowitall.paralex.ParalexClient
import com.typesafe.config.ConfigFactory
import java.net.SocketTimeoutException
import org.slf4j.LoggerFactory

class ParalexTransition(client: ParalexClient = ParalexTransition.defaultClient, 
    skipTimeouts: Boolean = ParalexTransition.defaultSkipTimeouts) extends Transition[QaState, QaAction] {
  
  private val logger = LoggerFactory.getLogger(this.getClass)
  
  override def apply(s: QaState) = s match {
    case qs: QuestionState => parse(qs)
    case _ => Nil
  }
  def parse(qs: QuestionState) = {
    val q = qs.question
    for {
      record <- try {
        client.parse(q)
      } catch {
        case e: SocketTimeoutException => if (skipTimeouts) {
          logger.warn(s"Timed out on question $qs: $e")
          Nil
        } else {
          throw e
        }
      }
      query = record.query
      queryState = QueryState(query)
    } yield (record, queryState)
  }
}

object ParalexTransition {
  val conf = ConfigFactory.load()
  val defaultSkipTimeouts = conf.getBoolean("paralex.skipTimeouts")
  lazy val defaultClient = ParalexClient()
}