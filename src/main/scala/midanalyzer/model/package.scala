package midanalyzer

import java.nio.file.Path

package object model {
  // заголовок MIDI-файла
  case class MidiHeader(format: Int, tracks: Int, division: Int) {
    def ticksPerQuarter: Int = division & 0x7FFF
    def isSMPTE: Boolean = (division & 0x8000) != 0
  }

  // MIDI-событие (до преобразования времени)
  sealed trait MidiEvent {
    def deltaTicks: Int
  }
  case class NoteOnEvent(deltaTicks: Int, channel: Int, pitch: Int, velocity: Int) extends MidiEvent
  case class NoteOffEvent(deltaTicks: Int, channel: Int, pitch: Int, velocity: Int) extends MidiEvent
  case class TempoEvent(deltaTicks: Int, microsecPerQuarter: Int) extends MidiEvent
  case class TimeSignatureEvent(deltaTicks: Int, numerator: Int, denominator: Int) extends MidiEvent
  case class MetaEvent(deltaTicks: Int, metaType: Int, data: Array[Byte]) extends MidiEvent
  case class UnknownEvent(deltaTicks: Int, status: Int, data: Array[Byte]) extends MidiEvent

  // преобразованная нота с абсолютным временем в секундах
  case class Note(startSec: Double, endSec: Double, pitch: Int, channel: Int, velocity: Int) {
    def durationSec: Double = endSec - startSec
    def pitchClass: Int = pitch % 12
  }

  // результат анализа
  case class AnalysisResult(
      filePath: String,
      totalNotes: Int,
      noteDensity: Double,        // нот в секунду
      bpm: Double,                // медиана интервалов между атаками -> BPM
      pitchClassProfile: Vector[Double], //  размер 12, нормализовано
      estimatedKey: String,       // например, "C major"
      chordHistogram: Map[String, Int],
      durationSec: Double,
      tracks: Int
  )

  // Конфигурация (может быть расширена)
  case class Config(
      chordWindowMs: Int = 120,
      minChordNotes: Int = 3,
      bpmMin: Double = 60,
      bpmMax: Double = 200
  )
}