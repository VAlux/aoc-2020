import scala.annotation.tailrec
import scala.util.chaining.*

def checkNumberIsValid(number: BigDecimal, preamble: List[BigDecimal]): Boolean =
  @tailrec
  def go(preamble: List[BigDecimal], lo: Int, hi: Int): Boolean =
    val sum = preamble(lo) + preamble(hi)
    if lo == 0 && hi == 0 then false
    else if sum < number then go(preamble, lo + 1, hi)
    else if sum > number then go(preamble, lo, hi - 1)
    else true

  go(preamble.sorted, 0, preamble.size - 1)

def locateWeakness(numbers: List[BigDecimal], preambleSize: Int): Option[BigDecimal] =
  numbers.sliding(preambleSize + 1).find(window => !checkNumberIsValid(window.last, window.init)).map(_.last)

def decode(weakNumber: BigDecimal, numbers: List[BigDecimal]): BigDecimal =
  @tailrec
  def go(elems: List[BigDecimal], index: Int, amount: Int): BigDecimal =
    val sum = elems.sum
    if sum == weakNumber then elems.max + elems.min
    else if sum < weakNumber then go(numbers.slice(index, index + amount + 1), index, amount + 1)
    else go(numbers.slice(index + 1, index + 3), index + 1, 2)

  go(numbers.slice(0, 2), 0, 2)

def solve(input: List[String]): BigDecimal =
  input
    .map(BigDecimal(_))
    .pipe(numbers => locateWeakness(numbers, 25).map(weak => decode(weak, numbers)).getOrElse(BigDecimal(-1)))

@main def entrypoint =
  println(solve(FileLoader.readFile("input.txt")))
