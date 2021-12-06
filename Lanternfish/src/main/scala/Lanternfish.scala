import scala.io.Source
import scala.util.Using

object Lanternfish {
  def main(args: Array[String]): Unit = {
    val input = Using.Manager { use => use(Source.fromFile("src/main/input.txt")).getLines.flatMap(line => line.split(',').map(_.toInt)).toList }.get
    val state = (0 to 8).map[Long](i => input.count(_ == i))
    println(getPopulationCount(80)(state), getPopulationCount(256)(state))
  }

  def getPopulationCount(days: Int)(state: Seq[Long]): Long =
    (1 to days).foldLeft(state) { case (state, _) => (state.tail :+ state.head).updated(6, state(7) + state.head) }.sum
}
