import scala.io.Source

object Day6 extends App {

  type Answers = Array[Char]

  case class Group(answers: List[Answers]) {
    val sharedAnswers: Set[Char] =
      answers.flatten.groupBy(x => x).filter { case (_, v) => v.length == answers.length }.keySet

    val distinctAnswers: Set[Char] = answers.flatten.toSet
  }

  val groups = Source.fromResource("input_day_6.txt")
    .getLines
    .mkString("\n")
    .split("\n\n")
    .map(in => Group(in.split("\n").map(_.toCharArray).toList))

  val distinctAnswers = groups.map(_.distinctAnswers.size).sum
  val sharedAnswers = groups.map(_.sharedAnswers.size).sum

  println(s"Part 1: $distinctAnswers")
  println(s"Part 2: $sharedAnswers")
}
