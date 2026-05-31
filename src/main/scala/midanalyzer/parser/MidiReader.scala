package midanalyzer.parser

import zio.ZIO
import java.io.{FileInputStream, InputStream}
import scala.collection.mutable.ArrayBuffer
import midanalyzer.model._
import midanalyzer.parser.VLQ.readVLQ

object MidiReader {
  def readFile(path: String): ZIO[Any, String, (MidiHeader, Vector[MidiEvent])] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt(new FileInputStream(path)).mapError(_.toString)
    )(
      is => ZIO.succeed(is.close()).ignore
    ) { is =>
      for {
        header <- readHeader(is)
        events <- readAllTracks(is, header)
      } yield (header, events)
    }
  }

  private def readHeader(is: InputStream): ZIO[Any, String, MidiHeader] = ZIO.attemptBlocking {
    val chunk = new Array[Byte](4)
    is.read(chunk)
    if (!new String(chunk).equals("MThd")) throw new Exception("Not a MIDI file")
    val length = readInt(is)
    if (length < 6) throw new Exception("Invalid header length")
    val format = readShort(is)
    val tracks = readShort(is)
    val division = readShort(is)
    MidiHeader(format, tracks, division)
  }.mapError(_.toString)

  private def readInt(is: InputStream): Int = {
    ((is.read() & 0xFF) << 24) | ((is.read() & 0xFF) << 16) |
      ((is.read() & 0xFF) << 8) | (is.read() & 0xFF)
  }

  private def readShort(is: InputStream): Int = ((is.read() & 0xFF) << 8) | (is.read() & 0xFF)

  private def readAllTracks(is: InputStream, header: MidiHeader): ZIO[Any, String, Vector[MidiEvent]] = {
    def readOneTrack: ZIO[Any, String, Vector[MidiEvent]] = ZIO.attemptBlocking {
      val chunk = new Array[Byte](4)
      is.read(chunk)
      if (!new String(chunk).equals("MTrk")) throw new Exception("Expected MTrk chunk")
      val length = readInt(is)
      val trackData = new Array[Byte](length)
      is.read(trackData)
      parseTrack(trackData)
    }.mapError(_.toString)

    ZIO.foreach(1 to header.tracks)(_ => readOneTrack).map(_.flatten.toVector)
  }

  private def parseTrack(data: Array[Byte]): Vector[MidiEvent] = {
    val events = ArrayBuffer.empty[MidiEvent]
    var pos = 0
    var runningStatus = 0
    while (pos < data.length) {
      val delta = readVLQFromArray(data, pos)
      pos += delta.bytesRead
      val statusByte = data(pos) & 0xFF
      pos += 1
      val command = if ((statusByte & 0x80) != 0) {
        runningStatus = statusByte
        statusByte
      } else {
        pos -= 1
        runningStatus
      }
      val cmdHigh = command >> 4
      val channel = command & 0x0F
      cmdHigh match {
        case 0x8 => // Note Off
          val pitch = data(pos) & 0xFF
          val vel = data(pos + 1) & 0xFF
          pos += 2
          events += NoteOffEvent(delta.value, channel, pitch, vel)
        case 0x9 => // Note On
          val pitch = data(pos) & 0xFF
          val vel = data(pos + 1) & 0xFF
          pos += 2
          events += NoteOnEvent(delta.value, channel, pitch, vel)
        case 0xA => pos += 2
        case 0xB => pos += 2
        case 0xC => pos += 1
        case 0xD => pos += 1
        case 0xE => pos += 2
        case 0xF =>
          if (command == 0xFF) {
            val metaType = data(pos) & 0xFF
            pos += 1
            val len = readVLQFromArray(data, pos)
            pos += len.bytesRead
            val metaData = data.slice(pos, pos + len.value)
            pos += len.value
            metaType match {
              case 0x51 =>
                val tempo = (metaData(0) << 16) | (metaData(1) << 8) | metaData(2)
                events += TempoEvent(delta.value, tempo)
              case 0x58 =>
                events += TimeSignatureEvent(delta.value, metaData(0), metaData(1))
              case _ => events += MetaEvent(delta.value, metaType, metaData)
            }
          } else {
            val len = readVLQFromArray(data, pos)
            pos += len.bytesRead + len.value
          }
        case _ =>
          val len = readVLQFromArray(data, pos)
          pos += len.bytesRead + len.value
      }
    }
    events.toVector
  }

  private case class VLQResult(value: Int, bytesRead: Int)
  private def readVLQFromArray(data: Array[Byte], start: Int): VLQResult = {
    var value = 0
    var bytes = 0
    var byte = 0
    var continue = true
    while (continue) {
      byte = data(start + bytes) & 0xFF
      value = (value << 7) | (byte & 0x7F)
      bytes += 1
      if ((byte & 0x80) == 0) continue = false
    }
    VLQResult(value, bytes)
  }
}