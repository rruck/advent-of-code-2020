import scala.io.Source
import scala.util.chaining._
import scala.language.implicitConversions

object Day8 extends App {

  sealed trait Operation

  case object NOP extends Operation

  case class ACC(add: Int) extends Operation

  case class JMP(jmp: Int) extends Operation

  case class State(pos: Int = 0, acc: Int = 0)

  val toOperation: String => Operation = {
    case s"nop $_" => NOP
    case s"acc $n" => ACC(n.toInt)
    case s"jmp $n" => JMP(n.toInt)
  }

  val input = Source.fromResource("./input_day_8.txt").getLines.toArray


  val operations = input.map(toOperation)


  val execute: (State, Operation) => State = (state, op) => op match {
    case NOP => state.copy(pos = state.pos + 1)
    case ACC(add) => State(state.pos + 1, state.acc + add)
    case JMP(jmp) => state.copy(state.pos + jmp)
  }

  val endExecution: (Int, List[Int]) => Boolean  = (nextPos, history) => history.contains(nextPos)

  def run(state: State, history: List[Int] = List.empty): State = {
    val op = operations(state.pos)
    val newState = execute(state, op)
    if(endExecution(newState.pos, history))
      newState
    else
      run(newState, history :+ newState.pos)
  }

  val state = run(State())
  println(state)

}
