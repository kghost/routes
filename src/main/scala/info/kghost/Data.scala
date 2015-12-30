package info.kghost

import scalaj.http.HttpOptions
import scalaj.http.Http
import java.io.InputStream
import scala.io.Source
import java.nio.file.Files
import java.nio.file.Paths
import java.io.File
import org.apache.commons.io.IOUtils
import java.io.FileOutputStream

object Data {
  private val data = Map(
    "delegated-apnic-latest" -> "http://ftp.apnic.net/apnic/stats/apnic/delegated-apnic-latest",
    "delegated-apnic-extended-latest" -> "http://ftp.apnic.net/apnic/stats/apnic/delegated-apnic-extended-latest",
    "delegated-afrinic-latest" -> "http://ftp.afrinic.net/pub/stats/afrinic/delegated-afrinic-latest",
    "delegated-afrinic-extended-latest" -> "http://ftp.afrinic.net/pub/stats/afrinic/delegated-afrinic-extended-latest",
    "delegated-arin-extended-latest" -> "http://ftp.arin.net/pub/stats/arin/delegated-arin-extended-latest",
    "delegated-lacnic-latest" -> "http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-latest",
    "delegated-lacnic-extended-latest" -> "http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-latest",
    "delegated-ripencc-latest" -> "http://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-latest",
    "delegated-ripencc-extended-latest" -> "http://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-extended-latest")

  lazy val data1 = {
    data.values flatMap { url =>
      Http(url).option(HttpOptions.connTimeout(10000)).
        option(HttpOptions.readTimeout(60000)).
        asString.body.lines
    }
  }

  lazy val data2 = {
    data.keys flatMap { res =>
      val file = getClass.getResource(res).getContent.asInstanceOf[InputStream]
      Source.fromInputStream(file).getLines
    }
  }

  def main(args: Array[String]) {
    for ((k, v) <- data) {
      Http(v).option(HttpOptions.connTimeout(10000)).
        option(HttpOptions.readTimeout(60000)).execute {
          IOUtils.copy(_, new FileOutputStream(new File(getClass.getResource(k).toURI())))
        }
    }
  }
}
