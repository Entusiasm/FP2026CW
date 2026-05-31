package midanalyzer.analysis

import midanalyzer.model._
import scala.collection.mutable

object NoteExtractor {
  def extractNotes(events: Vector[MidiEvent], ticksPerQuarter: Int, initialBpm: Double = 120.0): (Vector[Note], Double, Vector[Double]) = {
    // Переводим тики в секунды с учётом изменений темпа
    var currentTempoUS = 500000 // по умолчанию 120 BPM = 500000 мкс на четверть
    var currentSeconds = 0.0
    var absoluteTicks = 0L
    val notesMap = mutable.Map.empty[(Int, Int), (Double, Int)] // (канал, высота) -> (время_старта, скорость)
    val notesList = mutable.ListBuffer.empty[Note]
    val tempoChanges = mutable.ListBuffer.empty[(Double, Double)] // (секунды, bpm)
    tempoChanges += ((0.0, 60000000.0 / currentTempoUS))

    for (event <- events) {
      // Продвинуть время
      val deltaSec = (event.deltaTicks.toDouble / ticksPerQuarter) * (currentTempoUS / 1_000_000.0)
      currentSeconds += deltaSec
      absoluteTicks += event.deltaTicks

      event match {
        case TempoEvent(_, usPerQuarter) =>
          currentTempoUS = usPerQuarter
          val bpm = 60000000.0 / usPerQuarter
          tempoChanges += ((currentSeconds, bpm))
        case NoteOnEvent(_, ch, pitch, vel) if vel > 0 =>
          notesMap((ch, pitch)) = (currentSeconds, vel)
        case NoteOnEvent(_, ch, pitch, vel) if vel == 0 => // рассматриваем как выключение ноты
          notesMap.get((ch, pitch)).foreach { case (start, velOn) =>
            notesList += Note(start, currentSeconds, pitch, ch, velOn)
            notesMap.remove((ch, pitch))
          }
        case NoteOffEvent(_, ch, pitch, _) =>
          notesMap.get((ch, pitch)).foreach { case (start, velOn) =>
            notesList += Note(start, currentSeconds, pitch, ch, velOn)
            notesMap.remove((ch, pitch))
          }
        case _ => // остальные игнорим
      }
    }
    // закрываем все «висящие» ноты
    for (((ch, pitch), (start, vel)) <- notesMap) {
      notesList += Note(start, currentSeconds, pitch, ch, vel)
    }
    (notesList.toVector, currentSeconds, tempoChanges.map(_._2).toVector)
  }
}