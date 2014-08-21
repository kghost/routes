package info.kghost

import scalaj.http.HttpOptions
import scalaj.http.Http
import java.io.InputStream
import scala.io.Source

object Data {
  lazy val data1 = {
    val urls = Array(
      "http://ftp.apnic.net/apnic/stats/apnic/delegated-apnic-latest",
      "http://ftp.apnic.net/apnic/stats/apnic/delegated-apnic-extended-latest",
      "http://ftp.afrinic.net/pub/stats/afrinic/delegated-afrinic-latest",
      "http://ftp.afrinic.net/pub/stats/afrinic/delegated-afrinic-extended-latest",
      "http://ftp.arin.net/pub/stats/arin/delegated-arin-extended-latest",
      "http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-latest",
      "http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-latest",
      "http://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-latest",
      "http://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-extended-latest")

    urls flatMap { url =>
      Http(url).option(HttpOptions.connTimeout(10000)).
        option(HttpOptions.readTimeout(60000)).
        asString.lines
    }
  }

  val data2 = {
    val resources = Array(
      "/delegated-apnic-latest",
      "/delegated-afrinic-latest",
      "/delegated-arin-extended-latest",
      "/delegated-lacnic-extended-latest",
      "/delegated-ripencc-latest")
    resources flatMap { res =>
      val file = getClass.getResource(res).getContent.asInstanceOf[InputStream]
      Source.fromInputStream(file).getLines
    }
  }
}