package iohk.scorex.difficulty

import java.io.{BufferedWriter, File, FileWriter}

import scala.BigInt._
import scala.concurrent.duration.{FiniteDuration, _}
import scala.io.Source
import scala.util.Random

object Simulator extends App {

  type HashRate = BigInt
  type Difficulty = BigInt
  type Duration = Long

  val Desired = 10.minutes
  val R = 1000
  val Ra = 200

  lazy val lines: Seq[String] = Source.fromFile(s"difficulties.txt").getLines().toList
  lazy val targets: IndexedSeq[BigInt] = lines.map(_.split(" ")).map(l => BigInt(l.last)).toIndexedSeq
  lazy val bitcoinDiffficulties: IndexedSeq[BigInt] = targets.map(t => targets(0) / t)
  val Epochs = bitcoinDiffficulties.length

//  println(bitcoinDiffficulties)

  Seq("linear", "bitcoin").foreach { diffControlString =>
    Seq("exponent", "linear", "attack", "real").foreach { hashRateString =>
      val diffControl = diffControlString match {
        case "linear" => new LinearDifficultyControl
        case "bitcoin" => new BitcoinDifficultyControl
        case _ => new BitcoinDifficultyControl
      }

      val hashRate: (Int, HashRate) => BigInt = hashRateString match {
        case "constant" => (i: Int, prev: HashRate) => BigInt(R)
        case "exponent" => (i: Int, prev: HashRate) => prev * 110 / 100
        case "linear" => (i: Int, prev: HashRate) => prev + 100
        case "attack" => (i: Int, prev: HashRate) => if (i % 2 == 1) R else R + Ra
        case "random" => (i: Int, prev: HashRate) => prev * (900 + Random.nextInt(204)) / 1000
        case "real" => (i: Int, prev: HashRate) => bitcoinDiffficulties(i)
      }

      def simulate(i: Int, acc: Seq[(Int, Difficulty, HashRate, FiniteDuration)], maxI: Int): Seq[(Int, Difficulty, HashRate, FiniteDuration)] = if (i < maxI) {
        val R = hashRate(i, acc.head._3)
        val newDiff = diffControl.diff(acc.take(50).map(a => a._2 -> a._4), Desired)
        val realDuration = (Desired.toMillis * newDiff / R).toInt.millis
        simulate(i + 1, (i, newDiff, R, realDuration) +: acc, maxI)
      } else acc

      val initialDiff: BigInt = if(hashRateString == "real") bitcoinDiffficulties(0)
      else BigInt(R)

      val res = simulate(1, Seq((0, initialDiff, initialDiff, Desired)), Epochs).reverse
      val file = new File(s"data/$diffControlString/$hashRateString.csv")
      new File(s"data/$diffControlString").mkdirs()
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write("Epoch,CalculatedHashRate,RealHashRate,RealDuration\n")
      //      println("Epoch,CalculatedHashRate,RealHashRate,RealDuration")
      res.foreach { r =>
        //        println(r._1 + "," + r._2 + "," + r._3 + "," + r._4.toMillis)
        bw.write(r._1 + "," + r._2 + "," + r._3 + "," + r._4.toMillis + "\n")
      }
      bw.write((res.map(_._4.toMillis).sum / res.length) + "\n")
      bw.close()

    }
  }


}



