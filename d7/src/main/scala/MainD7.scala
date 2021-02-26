
@main def entrypoint = 
  println(solve(FileLoader.readFile("input.txt")))

def solve(input: List[String]): Int = 0