package edu.knowitall.util

import java.io.InputStream

object ResourceUtils {
  def resource(path: String): InputStream = {
    val stream = getClass.getResourceAsStream(path)
    if (stream != null) {
      stream
    } else {
      throw new IllegalStateException(s"could not load resource $path")
    }
  }
}