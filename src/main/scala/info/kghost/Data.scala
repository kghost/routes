package info.kghost

import java.io.InputStream
import scala.io.Source

object Data {
  private val files = Array(
    "delegated-apnic-latest",
    "delegated-apnic-extended-latest",
    "delegated-afrinic-latest",
    "delegated-afrinic-extended-latest",
    "delegated-arin-extended-latest",
    "delegated-lacnic-latest",
    "delegated-lacnic-extended-latest",
    "delegated-ripencc-latest",
    "delegated-ripencc-extended-latest")

  lazy val data = {
    files flatMap { res =>
      val file = getClass.getResource("/" + res).getContent.asInstanceOf[InputStream]
      Source.fromInputStream(file).getLines
    }
  }
}
