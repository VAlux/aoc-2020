import scala.util.chaining._

@main def hello: Unit =
    println(run)

def run = 
  val input = FileLoader.readFile("input.txt").map(_.toInt)
  go(input.head, input.tail)

def go(elem: Int, rem: List[Int], target: Int = 2020): Int = rem match
    case Nil => 0
    case _ => rem
        .filter(lo => lo + elem < target)
        .flatMap(lo => rem.find(hi => hi + lo + elem == target).map(hi => hi * lo * elem))
        .headOption.getOrElse(go(rem.head, rem.tail))
