package iohk.scorex.difficulty

import iohk.scorex.difficulty.Simulator.Difficulty

import scala.concurrent.duration.FiniteDuration

class LinearDifficultyControl(useLastEpochs: Int = 4) extends DifficultyControl {

  override def diff(lastDiffs: Seq[(Difficulty, FiniteDuration)], desired: FiniteDuration): BigInt = {
    if (lastDiffs.size >= 2) {
      val realDiffs: Seq[BigInt] = lastDiffs.reverse.map(l => l._1 * desired.toMillis / l._2.toMillis).toSeq
      val data: Seq[(Int, Difficulty)] = realDiffs.indices.map(i => i -> realDiffs(i))
      interpolate(data.takeRight(useLastEpochs))(data.map(_._1).max + 1)
    } else lastDiffs.last._1
  }

  //y = a + bx
  def interpolate(data: Seq[(Int, BigInt)]): (Int) => BigInt = {
    val size = data.size
    val xy: Iterable[BigInt] = data.map(d => d._1 * d._2)
    val x: Iterable[Int] = data.map(d => d._1)
    val x2: Iterable[Int] = data.map(d => d._1 * d._1)
    val y: Iterable[BigInt] = data.map(d => d._2)
    val xyMean = BigDecimal(xy.sum) / size
    val x2Mean = BigDecimal(x2.sum) / size
    val yMean = BigDecimal(y.sum) / y.size
    val xMean = BigDecimal(x.sum) / x.size

    val k: BigDecimal = (xyMean - xMean * yMean) / (x2Mean - xMean* xMean)
    val b: BigDecimal = yMean - k * xMean
    (point: Int) => {
      (b + k * point).toBigInt()
    }
  }

}