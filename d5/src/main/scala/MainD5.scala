import scala.util.chaining._
import scala.annotation.tailrec

@main def entrypoint = 
  println(solve(FileLoader.readFile("input.txt")))

def solve(input: List[String]) = 
  input
    .map(pass => pass.partition(ch => ch == 'F' || ch == 'B'))
    .map { case (fb, rl) => (split(fb.toCharArray.toList, 0, 127) * 8) + split(rl.toCharArray.toList, 0, 7) }
    .sorted
    .sliding(2, 1)
    .find(window => window.last - window.head != 1)
    .map(window => window.last - 1)
    .getOrElse(-1)

@tailrec
def split(current: List[Char], lo: Int, hi: Int): Int = current match
    case last :: Nil => last match
      case 'F' | 'L' => lo
      case 'B' | 'R' => hi
    case head :: tail => head match
      case 'F' | 'L' => split(tail, lo, Math.floor(lo + (hi - lo) / 2d).toInt)
      case 'B' | 'R' => split(tail, Math.round(lo + (hi - lo) / 2d).toInt, hi)