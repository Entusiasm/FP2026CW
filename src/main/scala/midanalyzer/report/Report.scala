package midanalyzer.report

import midanalyzer.model.AnalysisResult
import zio.ZIO
import java.nio.file.{Files, Paths}

object Report {
  def toJson(result: AnalysisResult): String = {
    s"""{
       |  "version": 1,
       |  "file": "${escape(result.filePath)}",
       |  "duration_sec": ${result.durationSec},
       |  "total_notes": ${result.totalNotes},
       |  "note_density": ${result.noteDensity},
       |  "bpm": ${result.bpm},
       |  "estimated_key": "${escape(result.estimatedKey)}",
       |  "pitch_class_profile": [${result.pitchClassProfile.mkString(", ")}],
       |  "tracks": ${result.tracks}
       |}""".stripMargin
  }

  def toText(result: AnalysisResult): String = {
  s"""MIDI Analyzer Report
     |=====================
     |File: ${result.filePath}
     |Duration: ${result.durationSec} sec
     |Tracks: ${result.tracks}
     |Total notes: ${result.totalNotes}
     |Note density: ${result.noteDensity} notes/sec
     |Estimated tempo (BPM): ${result.bpm}
     |Estimated key: ${result.estimatedKey}
     |Pitch class profile: ${result.pitchClassProfile.map(p => f"$p%.3f").mkString(", ")}
     |""".stripMargin
  }

  def saveJson(result: AnalysisResult, path: String): ZIO[Any, String, Unit] = {
    ZIO.attemptBlocking {
      Files.write(Paths.get(path), toJson(result).getBytes("UTF-8"))
    }.unit.mapError(_.toString)
  }

  def saveText(result: AnalysisResult, path: String): ZIO[Any, String, Unit] = {
    ZIO.attemptBlocking {
      Files.write(Paths.get(path), toText(result).getBytes("UTF-8"))
    }.unit.mapError(_.toString)
  }

  private def escape(s: String): String = s.replace("\\", "\\\\").replace("\"", "\\\"")
  private def mapToJson(m: Map[String, Int]): String = {
    m.toList.map { case (k, v) => s""""$k": $v""" }.mkString("{", ", ", "}")
  }
}