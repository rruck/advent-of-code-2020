import scala.io.Source
import scala.util.chaining._

object Day7 extends App {

  type BagColor = String
  val input = Source.fromResource("./input_day_7.txt").getLines

  object Part1 {

    case class Rule(bag: BagColor, contains: List[BagColor])

    val parseBag: String => List[BagColor] = _.split(", ").map {
      case s"1 $color bag" => color
      case s"$_ $color bags" => color
    }.toList

    val toRule: String => Rule = {
      case s"$color bags contain no other bags." => Rule(color, List.empty)
      case s"$color bags contain $containedBags." => Rule(color, parseBag(containedBags))
    }

    def invert[A, B](in: Map[A, List[B]]): Map[B, List[A]] =
      in.foldLeft(Map[B, List[A]]().withDefaultValue(List())) {
        case (agg, (k, v)) => v.foldLeft(agg)((map, b) => map.updated(b, agg(b) :+ k))
      }

    def traverse(bagColor: BagColor, visited: Set[BagColor] = Set.empty): Set[BagColor] = {
      isContainedIn.get(bagColor) match {
        case None => visited
        case Some(value) => value.toSet.foldRight(visited) { (color, vis) => if (vis.contains(color)) vis else vis ++ traverse(color, vis + color) }
      }
    }

    private val isContainedIn = input
      .toList
      .map(toRule)
      .groupMapReduce(r => r.bag)(r => r.contains)(_ ++ _)
      .pipe(invert)

    def run(): Unit = {


      val shinyGold = "shiny gold"

      println(traverse(shinyGold).size)

    }
  }

  object Part2 {

    case class Spec(count: Int, bag: BagColor)

    case class Rule(bag: BagColor, contains: List[Spec])

    val parseBag: String => List[Spec] = _.split(", ").map {
      case s"1 $color bag" => Spec(1, color)
      case s"$count $color bags" => Spec(count.toInt, color)
    }.toList

    val toRule: String => Rule = {
      case s"$color bags contain no other bags." => Rule(color, List.empty)
      case s"$color bags contain $containedBags." => Rule(color, parseBag(containedBags))
    }

    def run(): Unit = {
      val graph = input
        .toList
        .map(toRule)
        .groupMapReduce(r => r.bag)(r => r.contains)(_ ++ _)

      def collect(bagColor: BagColor): Long =
        graph.get(bagColor) match {
          case Some(specs) => specs.map { s => s.count + s.count * collect(s.bag) }.sum
          case None => 1
        }

      println(collect("shiny gold"))
    }
  }

  Part1.run()
  Part2.run()

}
