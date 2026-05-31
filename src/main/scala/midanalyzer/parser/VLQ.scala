package midanalyzer.parser

import zio.ZIO
import java.io.InputStream

object VLQ {
  def readVLQ(is: InputStream): ZIO[Any, String, Int] = ZIO.attemptBlocking {
    var value = 0
    var byte = 0
    var continue = true
    while (continue) {
      byte = is.read()
      if (byte == -1) throw new Exception("Unexpected end of stream while reading VLQ")
      value = (value << 7) | (byte & 0x7F)
      if ((byte & 0x80) == 0) continue = false
    }
    value
  }.mapError(_.toString)
}