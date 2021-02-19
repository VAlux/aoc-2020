@main def entrypoint = 
  println(solve(FileLoader.readFile("input.txt")))

def solve(input: List[String]): Int = 
  val rows = input.map(_.toCharArray)
  go(rows(1), rows.drop(1))

def go(currentRow: Array[Char], rem: List[Array[Char]], column: Int = 3, trees: Int = 0): Int = rem match
  case Nil => trees
  case head :: tail if currentRow(column) == '#' => go(head, tail, calcColumn(column, currentRow.size), trees + 1)
  case _ => go(rem.head, rem.tail, calcColumn(column, currentRow.size), trees)

def calcColumn(column: Int, rowLength: Int): Int = 
  if column + 3 >= rowLength then 3 - (rowLength % column)
  else column + 3
