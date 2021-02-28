case class GraphEdge(val value: Int, val target: GraphVertex)
case class GraphVertex(val color: String, val adjacentEdges: Set[GraphEdge])

@main def entrypoint = 
  println(solve(FileLoader.readFile("input_test.txt")))

def solve(input: List[String]) = 
  val lines = input
    .map(line => line.split("contain").toList)
    .map(_.filter(_ == "bags").map(_.trim))

  parse(lines.head, lines.tail)

def parse(current: List[String], rem: List[List[String]], vertices: Set[GraphVertex] = Set.empty) = 
  current match
    case bag :: contents :: Nil => 
      GraphVertex(bag.trim, parseEdges(contents.replace(".", "").split(",").toList, vertices))

  def parseEdges(bagContent: List[String], vertices: Set[GraphVertex]): Set[GraphEdge] = 
    go(bagContent.head, bagContent.tail, vertices)

  def go(current: String, rem: List[String], vertices: Set[GraphVertex], edges: Set[GraphEdge] = Set.empty): Set[GraphEdge] =
    rem match 
      case Nil => edges
      case _ => current.partition(_.isDigit) match
        case (amount, color) => 
          vertices.find(_.color == color) match
            case Some(existing) =>
              val newEdge = GraphEdge(amount.toInt, existing)
              go(rem.head, rem.tail, vertices, edges + newEdge)
            case None =>
              val newVertex = GraphVertex(color, Set.empty)
              val newEdge = GraphEdge(amount.toInt, newVertex)
              go(rem.head, rem.tail, vertices + newVertex, edges + newEdge)