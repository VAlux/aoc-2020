import scala.annotation.tailrec

def checkNumberIsValid(number: BigDecimal, preamble: List[BigDecimal]): Boolean =
  @tailrec
  def go(preamble: List[BigDecimal], lo: Int, hi: Int): Boolean =
    val sum = preamble(lo) + preamble(hi)
    if lo == 0 && hi == 0 then false
    else if sum < number then go(preamble, lo + 1, hi)
    else if sum > number then go(preamble, lo, hi - 1)
    else true

  go(preamble.sorted, 0, preamble.size - 1)

def decode(input: List[String], preambleSize: Int) =
  input
    .map(BigDecimal(_))
    .sliding(preambleSize + 1)
    .find(window => !checkNumberIsValid(window.last, window.init))
    .map(_.last)
    .getOrElse(BigDecimal(0))

def solve(input: List[String]): BigDecimal =
  decode(input, 25)

@main def entrypoint =
  println(solve(FileLoader.readFile("input.txt")))
