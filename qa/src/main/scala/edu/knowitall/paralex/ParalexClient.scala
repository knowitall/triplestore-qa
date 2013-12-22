package edu.knowitall.paralex

import scalaj.http.Http
import com.typesafe.config.ConfigFactory
import scalaj.http.HttpOptions

case class ParalexClient(url: String = ParalexClient.defaultUrl, timeout: Int = ParalexClient.defaultTimeout) {
  def parse(s: String) = {
    val resp = Http(url).option(HttpOptions.readTimeout(timeout))
    					.option(HttpOptions.connTimeout(timeout))
    					.param("sent", s).asString
    ParalexRecord.fromJson(resp)
  }
}

object ParalexClient {
  val conf = ConfigFactory.load()
  val defaultUrl = conf.getString("paralex.url")
  val defaultTimeout = conf.getInt("paralex.timeout")
}