package midanalyzer.analysis

import midanalyzer.model.Note

object Statistics {
  def computeNoteDensity(notes: Vector[Note], durationSec: Double): Double = {
    if (durationSec <= 0) 0.0 else notes.length.toDouble / durationSec
  }
}