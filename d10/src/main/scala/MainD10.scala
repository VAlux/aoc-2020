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

@main def entrypoint =
  val diff = calculateDiffSummary(FileLoader.readFile("input.txt"))
  println(diff.singleJoltDiffsCount * diff.tripleJoltDiffsCount)
