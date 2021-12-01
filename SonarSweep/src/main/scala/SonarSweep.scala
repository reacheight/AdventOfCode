import scala.io.Source
import scala.util.Using

object SonarSweep {
  def main(args: Array[String]): Unit = {
    val depths = Using.Manager { use => use(Source.fromFile("src/main/input.txt")).getLines().map(_.toInt).toList }.get

    val increasesCount: (Int, List[Int]) => Int = (window, depths) =>
      depths.drop(window).foldLeft((0, depths.take(window))) { case ((result, prev), current) =>(result + (if (current > prev.head) 1 else 0), prev.drop(1) :+ current) }._1

    println(increasesCount(1, depths))
    println(increasesCount(3, depths))
  }
}
