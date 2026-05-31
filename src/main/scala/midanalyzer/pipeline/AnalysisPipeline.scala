package midanalyzer.pipeline

import midanalyzer.model._
import midanalyzer.parser.MidiReader
import midanalyzer.analysis.{NoteExtractor, TempoAnalyzer, HarmonyAnalyzer, Statistics}
import zio.ZIO

object AnalysisPipeline {
  def analyze(path: String, config: Config): ZIO[Any, String, (AnalysisResult, List[String])] = {
    MidiReader.readFile(path).flatMap { case (header, events) =>
      val (notes, durationSec, _) = NoteExtractor.extractNotes(events, header.ticksPerQuarter)
      val bpm = TempoAnalyzer.computeBpm(notes)
      val pcp = HarmonyAnalyzer.computePitchClassProfile(notes)
      val key = HarmonyAnalyzer.estimateKey(pcp)
      val chords = HarmonyAnalyzer.detectChords(notes, config)
      val noteDensity = Statistics.computeNoteDensity(notes, durationSec)
      val totalNotes = notes.length
      val logs = List(
        s"File: $path",
        s"Tracks: ${header.tracks}, Format: ${header.format}",
        s"Duration: ${durationSec} sec",
        s"Total notes: $totalNotes",
        s"Note density: $noteDensity notes/sec",
        s"BPM: $bpm",
        s"Estimated key: $key",
      )
      ZIO.succeed((AnalysisResult(path, totalNotes, noteDensity, bpm, pcp, key, chords, durationSec, header.tracks), logs))
    }
  }
}