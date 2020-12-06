
import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.string.MatchesRegex
import scala.io.Source

final case class Passport(fields: List[PassportField]) {
  def fieldTypes: Set[PassportFieldType] = fields.map(_.fieldType).toSet
}

object Passport {
  val requiredFields: Set[PassportFieldType] = Set(
    PassportFieldType.byr,
    PassportFieldType.iyr,
    PassportFieldType.eyr,
    PassportFieldType.hgt,
    PassportFieldType.hcl,
    PassportFieldType.ecl,
    PassportFieldType.pid,
    PassportFieldType.cid,
  )
}

sealed trait PassportFieldType

object PassportFieldType {

  case object byr extends PassportFieldType
  case object iyr extends PassportFieldType
  case object eyr extends PassportFieldType
  case object hgt extends PassportFieldType
  case object hcl extends PassportFieldType
  case object ecl extends PassportFieldType
  case object pid extends PassportFieldType
  case object cid extends PassportFieldType

}



sealed abstract class PassportField(val fieldType: PassportFieldType)
object PassportField {

  type BirthYearValue = Int Refined Interval.Closed[1920, 2020]
  object BirthYearValue extends RefinedTypeOps[BirthYearValue, Int]

  type IssueYearValue = Int Refined Interval.Closed[2010, 2020]
  object IssueYearValue extends RefinedTypeOps[IssueYearValue, Int]

  type ExpirationYearValue = Int Refined Interval.Closed[2020, 2030]
  object ExpirationYearValue extends RefinedTypeOps[ExpirationYearValue, Int]

  type HairColorValue = String Refined MatchesRegex["""^#[0-9a-f]{6}$"""]
  object HairColorValue extends RefinedTypeOps[HairColorValue, String]

  type EyeColorValue = String Refined MatchesRegex["""^amb|blu|brn|gry|grn|hzl|oth$"""]
  object EyeColorValue extends RefinedTypeOps[EyeColorValue, String]

  type PassportIDValue = String Refined MatchesRegex["""^[0-9]{9}$"""]
  object PassportIDValue extends RefinedTypeOps[PassportIDValue, String]

  type CentimeterHeight = Int Refined Interval.Closed[150, 193]
  object CentimeterHeight extends RefinedTypeOps[CentimeterHeight, Int]
  type IncHeight = Int Refined Interval.Closed[59, 76]
  object IncHeight extends RefinedTypeOps[IncHeight, Int]

  sealed trait HeightValue
  case class Cm(value: CentimeterHeight) extends HeightValue
  case class In(value: IncHeight) extends HeightValue


  case class BirthYear(value: BirthYearValue) extends PassportField(PassportFieldType.byr)
  case class IssueYear(value: IssueYearValue) extends PassportField(PassportFieldType.iyr)
  case class ExpirationYear(value: ExpirationYearValue) extends PassportField(PassportFieldType.eyr)
  case class Height(value: HeightValue) extends PassportField(PassportFieldType.hgt)
  case class HairColor(value: HairColorValue) extends PassportField(PassportFieldType.hcl)
  case class EyeColor(value: EyeColorValue) extends PassportField(PassportFieldType.ecl)
  case class PassportID(value: PassportIDValue) extends PassportField(PassportFieldType.pid)
  case class CountryID(value: String) extends PassportField(PassportFieldType.cid)
}

object Day4 extends App {


  import PassportField._

  val rawPassports = Source.fromResource("./input_day_4.txt").getLines.mkString("\n").split("\n\n").toList

  val prepPassportFields: String => List[String] = _.replaceAll("\n", " ").split(" ").toList

  val validateHeight: String => Either[String, HeightValue] = {
    case s"${v}cm" => CentimeterHeight.from(v.toInt).map(Cm)
    case s"${v}in" => IncHeight.from(v.toInt).map(In)
    case invalid => Left(s"Invalid height $invalid")
  }

  val parseField: String => Either[String, PassportField] = {
    case s"byr:$value" => BirthYearValue.from(value.toInt).map(PassportField.BirthYear)
    case s"iyr:$value" => IssueYearValue.from(value.toInt).map(PassportField.IssueYear)
    case s"eyr:$value" => ExpirationYearValue.from(value.toInt).map(PassportField.ExpirationYear)
    case s"hgt:$value" => validateHeight(value).map(PassportField.Height)
    case s"hcl:$value" => HairColorValue.from(value).map(PassportField.HairColor)
    case s"ecl:$value" => EyeColorValue.from(value).map(PassportField.EyeColor)
    case s"pid:$value" => PassportIDValue.from(value).map(PassportField.PassportID)
    case s"cid:$value" => Right(PassportField.CountryID(value))
    case other => Left(s"Unknown field type $other")
  }

  val hasAllRequiredFields: Passport => Boolean = passport =>
    Passport.requiredFields.foldRight(true)((ft, agg) => ft match {
      case PassportFieldType.cid => agg
      case _ => agg && passport.fieldTypes.contains(ft)
    })


  def debugOutput(fields: List[String], invalidFields: List[String]) : Unit = {
    println(fields.mkString(" "))
    println(s"${invalidFields.map(s => s"-$s").mkString("\n")}")
    if(invalidFields.isEmpty) println("VALID")
    println("\n")
  }

  val parsePassport: List[String] => Passport = {
    fields =>
      val (invalidFields, validFields) = fields.map(parseField).partitionMap(identity)
      debugOutput(fields, invalidFields)
      Passport(validFields)
  }

  val passports = rawPassports
    .map(prepPassportFields)
    .map(parsePassport)

  val validPassports = passports.count(hasAllRequiredFields)


  println(s"Valid Passports: $validPassports")
}