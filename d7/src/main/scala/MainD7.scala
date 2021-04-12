import scala.util.chaining._
import scala.annotation.tailrec

type Graph[A] = Map[A, List[GraphEdge[A]]]
type SGraph = Graph[String]

case class GraphEdge[A](val weight: Int, val data: A)
case class ContainerContents(amount: Int, color: String)
case class ContainerDefinition(containerColor: String, contents: List[ContainerContents])

@main def entrypoint =
  val target = "shiny gold"
  val tracedPaths = solve(FileLoader.readFile("input_test2.txt"), target)
  // tracedPaths.foreach(path => println(showPathTo(path, target) + "\n\n"))
  tracedPaths.foreach(path => println(showPathFrom(path) + "\n\n"))
  val sum = tracedPaths.foldLeft(0L)((acc, path) => acc + weightSum(path))
  println(sum)

def solve(input: List[String], target: String) = 
  input
    .map(line => line.split("contain").toList)
    .map(_.map(_.replace("bags", "").replace("bag", "").replace(".", "").trim))
    .flatMap(parseContainerDefinitions)
    .pipe(buildGraph)
    // .pipe(graph => dfs(graph.removed(target), target))
    .pipe(graph => traceAllContents(graph, target))

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

def buildGraph(definitions: List[ContainerDefinition]): SGraph = 
  @tailrec
  def go(definition: ContainerDefinition, rem: List[ContainerDefinition], graph: SGraph = Map.empty): SGraph = 
    rem match
      case Nil => graph + generateEdges(definition)
      case head :: tail => go(rem.head, rem.tail, graph + generateEdges(definition))
  
  go(definitions.head, definitions.tail)

def generateEdges(definition: ContainerDefinition): (String, List[GraphEdge[String]]) = 
  (definition.containerColor -> definition.contents.map(content => GraphEdge(content.amount, content.color)))

def dfs(graph: SGraph, targetData: String): List[List[GraphEdge[String]]] = 
  def search(current: GraphEdge[String], path: List[GraphEdge[String]]): List[GraphEdge[String]] = 
    if current.data == targetData then path
    else graph(current.data).filterNot(path.contains).flatMap(edge => search(edge, path :+ current))

  graph
    .flatMap { case (value, edges) => edges.map(edge => search(edge, List(GraphEdge(edge.weight, value)))) }
    .toList
    .filter(_.size > 1)

def traceAllContents(graph: SGraph, startFrom: String): List[List[GraphEdge[String]]] = 
  def trace(current: GraphEdge[String], path: List[GraphEdge[String]]): List[GraphEdge[String]] = 
    graph.get(current.data) match 
      case Some(edges: List[GraphEdge[String]]) if !edges.isEmpty => 
        edges.filterNot(path.contains).flatMap(edge => trace(edge, path :+ current))
      case _ => 
        path :+ current

  graph(startFrom).map(edge => trace(edge, List(GraphEdge(1, startFrom)))).toList

def showPathTo(path: List[GraphEdge[String]], target: String): String =
 s"${path.map(_.data) mkString " -> "} -> $target"

def showPathFrom(path: List[GraphEdge[String]]): String =
  (path.sliding(2, 1).map {
    case (edge1 :: edge2 :: Nil) => edge1.data + s" --[${edge2.weight}]--> " 
    case _ => "###"
  } mkString "") + path.last.data

def weightSum(path: List[GraphEdge[String]]): Long = 
  path.sliding(2, 1).foldLeft(0)((a, b) => a + b.map(_.weight).reduce(_ * _))