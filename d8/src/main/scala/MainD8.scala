import scala.annotation.tailrec

case class Action(input: Int, term: Term)
case class State(counter: Int, instructionPointer: Int)

enum Term(val action: Int => State => State):
  case NOP extends Term(_ => state => state.copy(instructionPointer = state.instructionPointer + 1))
  case ACC extends Term(value => state => State(state.counter + value, state.instructionPointer + 1))
  case JMP extends Term(value => state => state.copy(instructionPointer = state.instructionPointer + value))

def run(actions: List[Action]): State = 
  @tailrec
  def go(currentAction: Action, currentState: State, visited: Set[Int] = Set.empty): State =
    println(s"$currentAction, $currentState")
    if visited.contains(currentState.instructionPointer) then currentState
    else 
      val newState = currentAction.term.action(currentAction.input)(currentState)
      go(actions(newState.instructionPointer), newState, visited + currentState.instructionPointer)
  
  go(actions.head, State(0, 0))

def parseTerm(term: String): Term =
  Term.values.find(_.toString.compareToIgnoreCase(term) == 0).getOrElse(Term.NOP)

def parseTermsList(input: List[String]): List[Action] = 
  input.flatMap { elem => 
    elem.split(" ").toList match 
      case term :: value :: Nil => Some(Action(value.toInt, parseTerm(term)))
      case _ => None
  }

def fixBrokenInstruction(actions: List[Action]): State = ???

def solve(input: List[String]): Int =
  run(parseTermsList(input)).counter

@main def entrypoint =
  println(solve(FileLoader.readFile("input.txt")))