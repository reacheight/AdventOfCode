import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object BinaryDiagnostic {
  def main(args: Array[String]): Unit = {
    val diagnostic = Using.Manager { use => use(Source.fromFile("src/main/input.txt")).getLines().map(_.toArray.map(_.asDigit)).toList}.get

    val maxCount = diagnostic.length
    val bitwiseOnesCount = diagnostic.foldLeft(Array.fill(diagnostic.head.length)(0))((count, number) => count.zip(number).map(pair => pair._1 + pair._2))
    val gamma = bitwiseOnesCount.map(count => if (count >= maxCount - count) 1 else 0)
    val epsilon = gamma.map(bit => (bit + 1) % 2)

    println(toDecimal(gamma) * toDecimal(epsilon))
    println(calculate((ones, zeroes) => ones >= zeroes)(diagnostic) * calculate((ones, zeroes) => ones < zeroes)(diagnostic))
  }

  def toDecimal(binary: Array[Int]): Int = Integer.parseInt(binary.mkString, 2)

  @tailrec
  def calculate(criteria: (Int, Int) => Boolean)(diagnostic: List[Array[Int]], index: Int = 0): Int = diagnostic match {
    case List(single) => toDecimal(single)
    case _ =>
      val (ones, zeroes) = diagnostic.partition(_(index) == 1)
      calculate(criteria)(if (criteria(ones.length, zeroes.length)) ones else zeroes, index + 1)
  }
}
