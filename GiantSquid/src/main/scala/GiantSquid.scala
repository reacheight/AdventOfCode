import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object GiantSquid {
  def main(args: Array[String]): Unit = {
    val input = Using.Manager { use => use(Source.fromFile("src/main/input.txt")).mkString.split("\r\n\r\n") }.get
    val numbers = input.head.split(',').map(_.toInt)
    val boards = input.drop(1).map(board => board.split('\n').map(row => row.trim.split(" +") .map(_.toIntOption)))

    println(calculateScore(firstWin = true)(boards, numbers))
    println(calculateScore(firstWin = false)(boards, numbers))
  }

  @tailrec
  def calculateScore(firstWin:  Boolean)(boards: Array[Array[Array[Option[Int]]]], numbers: Array[Int]): Int = {
    val marked = boards.map(board => board.map(row => row.map(number => number.find(_ != numbers.head))))
    val (won, others) = marked.partition(board => (board ++ board.transpose).exists(_.flatten.isEmpty))
    if (won.nonEmpty && firstWin || others.isEmpty && !firstWin)
      won.head.flatten.flatten.sum * numbers.head
    else
      calculateScore(firstWin)(others, numbers.tail)
  }
}
