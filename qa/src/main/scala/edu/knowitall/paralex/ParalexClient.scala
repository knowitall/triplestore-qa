package edu.knowitall.paralex

import scalaj.http.Http
import com.typesafe.config.ConfigFactory

case class ParalexClient(url: String = ParalexClient.defaultUrl) {
  def parse(s: String) = {
    val resp = Http(url).param("sent", s).asString
    ParalexRecord.fromJson(resp)
  }
}

object ParalexClient {
  val conf = ConfigFactory.load()
  val defaultUrl = conf.getString("paralex.url")
}