import scala.util.chaining._
import scala.annotation.tailrec

case class GraphEdge(val weight: Int, val data: String)

type Graph = Map[String, List[GraphEdge]]

@main def entrypoint =
  val target = "shiny gold"
  val tracedPaths = solve(FileLoader.readFile("input.txt"), target).distinctBy(path => path(0).data)
  tracedPaths.foreach(path => println(showPath(path, target) + "\n\n"))
  println(tracedPaths.size)

case class ContainerContents(amount: Int, color: String)
case class ContainerDefinition(containerColor: String, contents: List[ContainerContents])

def solve(input: List[String], target: String) = 
  input
    .map(line => line.split("contain").toList)
    .map(_.map(_.replace("bags", "").replace("bag", "").replace(".", "").trim))
    .flatMap(parseContainerDefinitions)
    .pipe(buildGraph)
    .pipe(graph => dfs(graph.removed(target), target))

def showAsGraphVizNotation(graph: Graph): String =
  s"${graph.map { case (value, edges) => "\"" + value + "\"" + s" -> ${edges.map(edge => "\"" + edge.data + "\"") mkString ","};\n" }}"

def showPath(path: List[GraphEdge], target: String): String =
 s"${path.map(_.data) mkString " -> "} -> $target"

def parseContainerDefinitions(definitions: List[String]): Option[ContainerDefinition] =
  definitions match
    case color :: contents :: Nil => Some(ContainerDefinition(color, parseContainerContents(contents)))
    case _ => None

def parseContainerContents(contents: String): List[ContainerContents] =
  contents.split(",").toList.flatMap { elem => 
    elem.partition(_.isDigit) match 
      case (amount, color) => 
        if color != "no other" then Some(ContainerContents(amount.toInt, color.trim))
        else None
  }

def buildGraph(definitions: List[ContainerDefinition]): Graph = 
  @tailrec
  def go(definition: ContainerDefinition, rem: List[ContainerDefinition], graph: Graph = Map.empty): Graph = 
    rem match
      case Nil => graph + generateEdges(definition)
      case head :: tail => go(rem.head, rem.tail, graph + generateEdges(definition))
  
  go(definitions.head, definitions.tail)

def generateEdges(definition: ContainerDefinition): (String, List[GraphEdge]) = 
  (definition.containerColor -> definition.contents.map(content => GraphEdge(content.amount, content.color)))

def dfs(graph: Graph, targetData: String): List[List[GraphEdge]] = 
  def trace(current: GraphEdge, path: List[GraphEdge]): List[GraphEdge] = 
    if current.data == targetData then path
    else graph(current.data).filterNot(path.contains).flatMap(edge => trace(edge, path :+ current))

  graph
    .flatMap { case (value, edges) => edges.map(edge => trace(edge, List(GraphEdge(edge.weight, value)))) }
    .toList
    .filter(_.size > 1)