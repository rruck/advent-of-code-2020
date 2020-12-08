import scala.io.Source
import scala.util.chaining._
import scala.language.implicitConversions

object Day8 extends App {

    sealed trait Operation
    case object NOP           extends Operation
    case class  ADD(add: Int) extends Operation
    case class  JMP(jmp: Int) extends Operation

    val toOperation: String => Operation = {
        case s"NOP" => NOP
        case s"ADD $n" => ADD(n.toInt)
        case s"JMP $n" => JMP(n.toInt)
    }

    val input = Source.fromResource("./input_day_8.txt").getLines



    input.map(toOperation).pipe(println)



}
