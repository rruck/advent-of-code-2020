import scala.io.Source

object Day5 extends App {

  type Row = Int
  type Column = Int
  type SeatId = Int
  type BinaryString = String

  case class BoardingPass(row: Row, column: Column) {
    val seatId: SeatId = row * 8 + column
  }

  val rawBoardingPasses = Source.fromResource("./input_day_5.txt").getLines

  val substitutions = Map('B' -> '1', 'F' -> '0', 'R' -> '1', 'L' -> '0')

  val toBinary: String => BinaryString = _.map(c => substitutions.getOrElse(c, c))

  val parseBinary: String => Int = raw => Integer.parseInt(toBinary(raw), 2)

  val parseBoardingPass: String => BoardingPass =  input =>
    BoardingPass(parseBinary(input.slice(0, 7)), parseBinary(input.slice(7, 10)))

  val missingNumber: List[SeatId] => Int = seats => (40 to 980).sum - seats.sorted.sum

  val seatIds: List[SeatId] = rawBoardingPasses.map(i => parseBoardingPass(i).seatId).toList
  println(s"max: ${seatIds.max}")

  val missing = missingNumber(seatIds)
  println(s"missing: $missing")

}
