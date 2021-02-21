import scala.annotation.tailrec
import scala.util.chaining._

/**
  * You start on the open square (.) 
  * in the top-left corner and need to reach the bottom (below the bottom-most row on your map). 
  * The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); 
  * start by counting all the trees you would encounter for the slope right 3, down 1: 
  * From your starting position at the top-left, check the position that is right 3 and down 1. 
  * Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.
  */
@main def entrypoint = 
  println(solve(FileLoader.readFile("input.txt")))
  println(solve(FileLoader.readFile("input_test.txt")))

case class Shift(val right: Int, val down: Int)

def solve(input: List[String], skip: Int = 1): BigDecimal = 
  List(Shift(1, 1), Shift(3, 1), Shift(5, 1), Shift(7, 1), Shift(1, 2))
    .map(shift => shiftDownSlope(input, shift))
    .tap(res => println(s"result: $res"))
    .foldLeft(BigDecimal.valueOf(1L))(_ * _)

def shiftDownSlope(input: List[String], shift: Shift): BigDecimal =
  val rows = inputToRows(input, shift)
  go(rows(shift.down), rows.drop(shift.down).tail, shift.right, shift.right, 0)

def inputToRows(input: List[String], shift: Shift): List[Array[Char]] = 
  if shift.down > 1 then input
    .map(_.toCharArray)
    .zipWithIndex
    .filter((row, index) => index > 0 && index % shift.down == 0)
    .map(_._1)
  else input.map(_.toCharArray)
  
@tailrec
def go(currentRow: Array[Char], rem: List[Array[Char]], column: Int, shift: Int, trees: BigDecimal): BigDecimal = 
  rem match
    case Nil => 
      if isTree(currentRow, column) then trees + 1 else trees
    case head :: tail if isTree(currentRow, column) => 
      go(head, tail, shiftRight(column, currentRow.size, shift), shift, trees + 1)
    case _ => 
      go(rem.head, rem.tail, shiftRight(column, currentRow.size, shift), shift, trees)

def shiftRight(column: Int, rowLength: Int, shift: Int): Int = (column + shift) % rowLength

def isTree(currentRow: Array[Char], column: Int): Boolean = currentRow(column) == '#'
