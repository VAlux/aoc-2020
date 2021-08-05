import scala.util.chaining._
import scala.annotation.tailrec

@main def entrypoint =
  println(solve(FileLoader.readFile("input.txt")))

def solve(input: List[String]): Int =
  parseAnswers(input).pipe(countSameAnswers)

def countSameAnswers(answers: List[List[String]]): Int =
  answers.map(_.map(_.toSet)).map(_.reduce(_ intersect _).size).sum

@tailrec
def parseAnswers(rows: List[String], answers: List[List[String]] = List.empty): List[List[String]] =
  if rows.isEmpty then answers.toList
  else
    val current = rows.takeWhile(_.nonEmpty)
    parseAnswers(rows.drop(current.size + 1), answers :+ current)
