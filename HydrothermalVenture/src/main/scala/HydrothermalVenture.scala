import scala.io.Source
import scala.util.Using
import scala.math.{abs, signum}

object HydrothermalVenture {
  def main(args: Array[String]): Unit = {
    val lines = Using.Manager { use =>
      use(Source.fromFile("src/main/input.txt"))
        .getLines()
        .map(line => toTuple(line.split(" -> ").map(c => toTuple(c.split(',').map(_.toInt)))))
        .toSeq
    }.get

    println(countIntersections(lines.filter { case ((x1, y1), (x2, y2)) => x1 == x2 || y1 == y2 }))
    println(countIntersections(lines))
  }

  def countIntersections(lines: Seq[((Int, Int), (Int, Int))]): Int =
    lines.foldLeft(Map.empty[(Int, Int), Int]) { case (linesCount, ((x1, y1), (x2, y2))) =>
      val xs = if (x1 == x2) Seq.fill(abs(y1 - y2) + 1)(x1) else x1 to x2 by signum(x2 - x1)
      val ys = if (y1 == y2) Seq.fill(abs(x1 - x2) + 1)(y1) else y1 to y2 by signum(y2 - y1)
      xs.zip(ys).foldLeft(linesCount) { case (linesCount, point) => linesCount.updatedWith(point)(value => Some(value.getOrElse(0) + 1)) }
    }.values.count(_ > 1)

  def toTuple[T](array: Array[T]): (T, T) = array match { case Array(first, second) => (first, second) }
}
