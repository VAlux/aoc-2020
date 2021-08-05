case class Action(input: Int, term: Term)
case class State(counter: Int, instructionPointer: Int)

enum Term(val action: Int => State => State):
  case NOP extends Term(_ => state => state.copy(instructionPointer = state.instructionPointer + 1))
  case ACC extends Term(value => state => State(state.counter + value, state.instructionPointer + 1))
  case JMP extends Term(value => state => state.copy(instructionPointer = state.instructionPointer + value))

def run(actions: List[Action]): State =
  def go(currentAction: Action, currentState: State, visited: Set[Int] = Set.empty): State =
    if visited.contains(currentState.instructionPointer) then currentState
    else
      val newState = currentAction.term.action(currentAction.input)(currentState)
      actions
        .lift(newState.instructionPointer)
        .map(action => go(action, newState, visited + currentState.instructionPointer))
        .getOrElse(currentState)

  go(actions.head, State(0, 0))

def parseTerm(term: String): Term =
  Term.values.find(_.toString.compareToIgnoreCase(term) == 0).getOrElse(Term.NOP)

def parseTermsList(input: List[String]): List[Action] =
  input.flatMap { elem =>
    elem.split(" ").toList match
      case term :: value :: Nil => Some(Action(value.toInt, parseTerm(term)))
      case _                    => None
  }

def fixBrokenInstruction(actions: List[Action]): Option[State] =
  def attempt(index: Int, value: Int, term: Term): Option[State] =
    val attempt = run(actions.updated(index, Action(value, term)))
    if attempt.instructionPointer >= actions.size - 1 then Some(attempt) else None

  actions.zipWithIndex.flatMap { (action, index) =>
    action match
      case Action(value, Term.NOP) => attempt(index, value, Term.JMP)
      case Action(value, Term.JMP) => attempt(index, value, Term.NOP)
      case _                       => None
  }.headOption

def solve(input: List[String]): Int =
  fixBrokenInstruction(parseTermsList(input)).map(_.counter).getOrElse(-1)

@main def entrypoint =
  println(solve(FileLoader.readFile("input.txt")))
