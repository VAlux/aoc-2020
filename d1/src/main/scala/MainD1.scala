@main def entrypoint: Unit =
    println(run)

def run = 
  val input = FileLoader.readFile("input.txt").map(_.toInt)
  locate(input.head, input.tail)

def locateV1(elem: Int, rem: List[Int], target: Int = 2020): Int =
  rem.find(_ + elem == target).headOption.map(_ * elem).getOrElse(locateV1(rem.head, rem.tail))

def locate(elem: Int, rem: List[Int], target: Int = 2020): Int = rem match
  case Nil => 0
  case _ => rem
    .filter(lo => lo + elem < target)
    .flatMap(lo => rem.find(hi => hi + lo + elem == target).map(hi => hi * lo * elem))
    .headOption.getOrElse(locate(rem.head, rem.tail))
