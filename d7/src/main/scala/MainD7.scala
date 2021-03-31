import scala.util.chaining._

case class GraphEdge(val weight: Int, val data: String)

type Graph = Map[String, Set[GraphEdge]]

@main def entrypoint = 
  println(solve(FileLoader.readFile("input_test.txt")) mkString "\n")

case class ContainerContents(amount: Int, color: String)
case class ContainerDefinition(containerColor: String, contents: List[ContainerContents])

def solve(input: List[String]) = 
  input
    .map(line => line.split("contain").toList)
    .map(_.map(_.replace("bags", "").replace("bag", "").replace(".", "").trim))
    .flatMap(parseContainerDefinitions)
    .pipe(buildGraph)

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
      case _ => None
  }

def buildGraph(definitions: List[ContainerDefinition]): Graph = 
  def go(definition: ContainerDefinition, rem: List[ContainerDefinition], graph: Graph = Map.empty): Graph = 
    rem match
      case Nil => graph + generateEdges(definition)
      case head :: tail => go(rem.head, rem.tail, graph + generateEdges(definition))
  
  go(definitions.head, definitions.tail)

def generateEdges(definition: ContainerDefinition): (String, Set[GraphEdge]) = 
  (definition.containerColor -> definition.contents.map(content => GraphEdge(content.amount, content.color)).toSet)