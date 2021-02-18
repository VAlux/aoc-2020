import scala.annotation.tailrec

/**
  For example, suppose you have the following list:

  1-3 a: abcde
  1-3 b: cdefg
  2-9 c: ccccccccc

  Each line gives the password policy and then the password. 
  The password policy indicates the lowest and highest number of times 
  a given letter must appear for the password to be valid. For example, 
  1-3 a means that the password must contain a at least 1 time and at most 3 times.

  In the above example, 2 passwords are valid. The middle password, cdefg, is not; 
  it contains no instances of b, but needs at least 1. 
  The first and third passwords are valid: they contain one a or nine c, 
  both within the limits of their respective policies.

  How many passwords are valid according to their policies?
 */

@main def entrypoint = 
  val input = FileLoader.readFile("input.txt")
  //println(solve(List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")))
  println(solve(input))

@tailrec
def solve(passwords: List[String], amount: Int = 0): Int = passwords match
  case Nil => amount
  case elem :: rem => elem.split(":").toList match
    case rule :: passwd :: Nil => 
      if applyRule(rule)(passwd) 
      then solve(rem, amount + 1) 
      else solve(rem, amount)
    case _ => amount

def applyRule(rule: String): String => Boolean =
  rule.split(" ").toList match 
    case range :: letter :: Nil => (input: String) => isValidV1(range, input.trim, letter(0))
    case _ => (input: String) => false
    
def isValidV1(range: String, input: String, letter: Char): Boolean = 
  range.split("-").toList.map(_.toInt) match 
    case from :: to :: Nil => 
      val count = input.count(ch => ch == letter)
      count >= from && count <= to
    case _ => false

def isValidV2(range: String, input: String, letter: Char): Boolean = 
  range.split("-").toList.map(_.toInt - 1) match 
    case from :: to :: Nil => input(from) == letter && input(to) != letter 
    case _ => false
  