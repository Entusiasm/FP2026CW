package midanalyzer.analysis

import midanalyzer.model.Note
import scala.collection.mutable.ArrayBuffer

object TempoAnalyzer {
  def computeBpm(notes: Vector[Note]): Double = {
    val attackTimes = notes.map(_.startSec).sorted
    if (attackTimes.length < 2) return 0.0
    val intervals = ArrayBuffer.empty[Double]
    for (i <- 1 until attackTimes.length) {
      val diff = attackTimes(i) - attackTimes(i-1)
      if (diff > 0.05 && diff < 2.0) intervals += diff // отфильтровываем выбросы
    }
    if (intervals.isEmpty) return 0.0
    val sorted = intervals.sorted
    val median = if (sorted.length % 2 == 0) (sorted(sorted.length/2-1) + sorted(sorted.length/2))/2.0
                 else sorted(sorted.length/2)
    val bpmRaw = 60.0 / median
    // нормализовываем в типичный музыкальный диапазон 70–180
    var bpm = bpmRaw
    while (bpm < 70) bpm *= 2
    while (bpm > 180) bpm /= 2
    bpm
  }
}