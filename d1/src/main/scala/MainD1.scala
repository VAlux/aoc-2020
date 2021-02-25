import scala.annotation.tailrec

@main def entrypoint: Unit =
    println(run)

def run = 
  val input = FileLoader.readFile("input.txt").map(_.toInt)
  // locate(input.head, input.tail)
  locateOpt(input.sorted, 0, input.size - 1)

@tailrec
def locateOpt(elems: List[Int], lo: Int, hi: Int): Int =
  val sum = elems(lo) + elems(hi)
  if sum == 2020 then elems(lo) * elems(hi) 
  else if sum < 2020 then locateOpt(elems, lo + 1, hi) 
  else locateOpt(elems, lo, hi - 1)

def locateV1(elem: Int, rem: List[Int], target: Int = 2020): Int =
  rem.find(_ + elem == target).headOption.map(_ * elem).getOrElse(locateV1(rem.head, rem.tail))

def locate(elem: Int, rem: List[Int], target: Int = 2020): Int = rem match
  case Nil => 0
  case _ => rem
    .filter(lo => lo + elem < target)
    .flatMap(lo => rem.find(hi => hi + lo + elem == target).map(hi => hi * lo * elem))
    .headOption.getOrElse(locate(rem.head, rem.tail))
