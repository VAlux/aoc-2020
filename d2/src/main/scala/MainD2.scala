import scala.annotation.tailrec

@main def entrypoint =
  val input = FileLoader.readFile("input.txt")
  println(solve(input))

@tailrec
def solve(passwords: List[String], amount: Int = 0): Int = passwords match
  case Nil => amount
  case elem :: rem =>
    elem.split(":").toList match
      case rule :: passwd :: Nil =>
        if applyRule(rule)(passwd) then solve(rem, amount + 1)
        else solve(rem, amount)
      case _ => amount

def applyRule(rule: String): String => Boolean =
  rule.split(" ").toList match
    case range :: letter :: Nil => (input: String) => isValidV2(range, input.trim, letter(0))
    case _                      => (input: String) => false

def isValidV1(range: String, input: String, letter: Char): Boolean =
  range.split("-").toList.map(_.toInt) match
    case from :: to :: Nil =>
      val count = input.count(ch => ch == letter)
      count >= from && count <= to
    case _ => false

def isValidV2(range: String, input: String, letter: Char): Boolean =
  range.split("-").toList.map(_.toInt - 1) match
    case from :: to :: Nil => (input(from) == letter) ^ (input(to) == letter)
    case _                 => false
