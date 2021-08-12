import scala.annotation.tailrec

case class DiffSummary(singleJoltDiffsCount: Int, tripleJoltDiffsCount: Int)

def calculateDiffSummary(input: List[String]) =
  def diffList(list: List[Int]): Int =
    list match
      case a :: b :: Nil => b - a
      case _             => -1

  val adapters = input.map(_.toInt).sorted
  val chain = 0 +: adapters :+ (adapters.last + 3)
  val diffs = chain.sliding(2).map(diffList).toList

  DiffSummary(diffs.count(_ == 1), diffs.count(_ == 3))

def trace(input: List[String]): List[List[Int]] =
  def pass(current: Int, rem: List[Int]): List[Int] =
    rem.filter(a => a - current == 1)
      ++ rem.filter(a => a - current == 2)
      ++ rem.filter(a => a - current == 3)

  @tailrec
  def go(current: List[Int], rem: List[Int], acc: List[List[Int]] = List.empty): List[List[Int]] =
    rem match
      case _ :: Nil => acc
      case _ :: xs =>
        val alternatives = current.flatMap(c => pass(c, xs))
        if alternatives.nonEmpty then go(alternatives, xs, acc :+ alternatives)
        else go(List(xs.head), xs.tail, acc :+ current)
      case _ => acc

  val adapters = input.map(_.toInt).sorted
  val device = (adapters.last + 3)
  val chain = 0 +: adapters :+ device

  go(List(chain.head), chain.tail)

def transpose(list: List[List[Int]]): List[List[Int]] = list.filter(_.nonEmpty) match
  case Nil => Nil
  case xs  => xs.map(_.head) :: transpose(xs.map(_.tail))

@main def entrypoint =
  val input = FileLoader.readFile("input_test.txt")
  // val diff = calculateDiffSummary(input)
  // println(diff.singleJoltDiffsCount * diff.tripleJoltDiffsCount)
  println(transpose(trace(input)) mkString "\n")
