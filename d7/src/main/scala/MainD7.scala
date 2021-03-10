case class GraphEdge(val value: Int, val target: GraphVertex)
case class GraphVertex(val color: String, val adjacentEdges: Set[GraphEdge])

@main def entrypoint = 
  println(solve(FileLoader.readFile("input_test.txt")) mkString "\n")

def solve(input: List[String]) = 
  val lines = input
    .map(line => line.split("contain").toList)
    .map(_.map(_.replace("bags", "").replace("bag", "").replace(".", "").trim))

  parse(lines.head, lines.tail)

def parse(current: List[String], rem: List[List[String]], vertices: Set[GraphVertex] = Set.empty): Set[GraphVertex] = 
  rem match 
    case Nil => vertices
    case _ => current match
      case bag :: contents :: Nil => 
        val content = contents.replace(".", "").split(",").map(_.trim).toList
        val (edges, newVertices) = parseEdges(content.head, content.tail, vertices)
        val newVertex = GraphVertex(bag.trim, edges)
        parse(rem.head, rem.tail, vertices ++ newVertices + newVertex)
      case _ => vertices

def parseEdges(current: String, rem: List[String], vertices: Set[GraphVertex], edges: Set[GraphEdge] = Set.empty): (Set[GraphEdge], Set[GraphVertex]) =
  rem match 
    case Nil => (edges, vertices)
    case _ => current.partition(_.isDigit) match
      case (amount, color) => 
        vertices.find(_.color == color) match
          case Some(existing) =>
            val newEdge = GraphEdge(amount.toInt, existing)
            parseEdges(rem.head, rem.tail, vertices, edges + newEdge)
          case None =>
            val newVertex = GraphVertex(color, Set.empty)
            val newEdge = GraphEdge(amount.toInt, newVertex)
            val newVertices = vertices + newVertex
            parseEdges(rem.head, rem.tail, newVertices, edges + newEdge)