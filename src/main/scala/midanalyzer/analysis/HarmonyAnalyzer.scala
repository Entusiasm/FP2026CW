package midanalyzer.analysis

import midanalyzer.model.{Note, Config}

object HarmonyAnalyzer {
  private val majorProfile = Vector(6.35, 2.23, 3.48, 2.52, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  private val minorProfile = Vector(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

  def computePitchClassProfile(notes: Vector[Note]): Vector[Double] = {
    val counts = Array.fill(12)(0.0)
    for (note <- notes) {
      counts(note.pitchClass) += note.durationSec
    }
    val total = counts.sum
    if (total == 0) return Vector.fill(12)(1.0 / 12)
    counts.map(_ / total).toVector
  }

  def estimateKey(pcp: Vector[Double]): String = {
    val keys = List("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
    var bestKey = "C major"
    var bestCorr = -1.0
    for (shift <- 0 until 12) {
      val rotatedMajor = majorProfile.drop(shift) ++ majorProfile.take(shift)
      val corrMajor = correlation(pcp, rotatedMajor)
      if (corrMajor > bestCorr) {
        bestCorr = corrMajor
        bestKey = s"${keys(shift)} major"
      }
      val rotatedMinor = minorProfile.drop(shift) ++ minorProfile.take(shift)
      val corrMinor = correlation(pcp, rotatedMinor)
      if (corrMinor > bestCorr) {
        bestCorr = corrMinor
        bestKey = s"${keys(shift)} minor"
      }
    }
    bestKey
  }

  private def correlation(a: Vector[Double], b: Vector[Double]): Double = {
    val n = a.length
    val sumA = a.sum
    val sumB = b.sum
    val sumAB = a.zip(b).map { case (x, y) => x * y }.sum
    val sumA2 = a.map(x => x * x).sum
    val sumB2 = b.map(x => x * x).sum
    val numerator = sumAB - sumA * sumB / n
    val denom = math.sqrt((sumA2 - sumA * sumA / n) * (sumB2 - sumB * sumB / n))
    if (denom == 0) 0 else numerator / denom
  }

    private def findTriad(pcs: Vector[Int]): Option[String] = {
    val rootCandidates = pcs.flatMap { root =>
      val major = Set(root, (root + 4) % 12, (root + 7) % 12)
      val minor = Set(root, (root + 3) % 12, (root + 7) % 12)
      if (major.subsetOf(pcs.toSet)) Some(s"${rootToName(root)} major")
      else if (minor.subsetOf(pcs.toSet)) Some(s"${rootToName(root)} minor")
      else None
    }
    rootCandidates.headOption
  }

  private def rootToName(pc: Int): String = List("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")(pc)
}