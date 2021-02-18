@main def entrypoint = 
  val input = FileLoader.readFile("input.txt")
  println(solve(input))

/**
 * 1-3 a: abcde
 * 1-3 b: cdefg
 * 2-9 c: ccccccccc
*/
def solve(passwords: List[String], amount: Int = 0): Int = passwords match
  case Nil => amount
  case elem :: rem => elem.split(":").toList match
    case rule :: passwd :: Nil => 
      if applyRule(rule)(passwd) 
      then solve(passwords.tail, amount + 1) 
      else solve(passwords.tail, amount)
    case _ => amount

def applyRule(rule: String): String => Boolean =
  rule.split(" ").toList match 
    case range :: letter :: Nil => (input: String) => isInRange(range, input.count(ch => ch == letter(0)))
    case _ => (input: String) => false

def isInRange(range: String, count: Int): Boolean = 
  range.split("-").toList.map(_.toInt) match 
    case from :: to :: Nil => count >= from && count <= to
    case _ => false
  