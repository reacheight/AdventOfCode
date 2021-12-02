import scala.io.Source
import scala.util.Using

object Dive {
  def main(args: Array[String]): Unit = {
    val commands = Using.Manager { use =>
      use(Source.fromFile("src/main/input.txt"))
        .getLines()
        .map(command => command.split(' ') match {case Array(direction, delta) => (direction, delta.toInt)})
        .toList
    }.get

    val (length, depth, aim) = commands.foldLeft((0, 0, 0)) {case ((length, depth, aim), (direction, delta)) => direction match {
      case "forward" => (length + delta, depth + aim * delta, aim)
      case "down" => (length, depth, aim + delta)
      case "up" => (length, depth, aim - delta)
    }}

    println(length * aim, length * depth)
  }
}
