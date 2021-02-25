import scala.util.chaining._
import scala.annotation.tailrec

@main def entrypoint = 
  println(solve(FileLoader.readFile("input.txt")))

case class PassportClaim(val name: String, val value: String)

val mandatoryFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
val eyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

def solve(input: List[String]): Int = 
  splitInputToRawPassports(input).map(parsePassportElements).count(passportClaimsAreValid)
  
@tailrec
def splitInputToRawPassports(rows: List[String], passports: List[String] = List.empty): List[String] = 
  if rows.isEmpty then passports.toList
  else 
    val current = rows.takeWhile(_.nonEmpty) 
    splitInputToRawPassports(rows.drop(current.size + 1), passports :+ (current mkString " "))

def parsePassportElements(rawPassport: String): List[PassportClaim] =
  rawPassport.split(" ").toList.flatMap { claimValue => 
    claimValue.split(":").toList match 
      case claim :: value :: Nil => Some(PassportClaim(claim, value))
      case _ => None
    }

def passportContainsAllMandatoryClaims(claims: List[PassportClaim]): Boolean = 
  mandatoryFields.diff(claims.map(_.name).toSet).toList match 
    case Nil => true
    case "cit" :: Nil => true
    case _ => false

def passportClaimsAreValid(claims: List[PassportClaim]): Boolean = 
  passportContainsAllMandatoryClaims(claims) && claims.forall(passportClaimIsValid)

def checkNumberInRange(num: String, min: Int, max: Int): Boolean =
  num.toIntOption.map(n => n >= min && n <= max).getOrElse(false)

def validateHeight(height: String): Boolean =
  height.partition(_.isDigit) match 
    case (digits, id) => id match
      case "cm" => checkNumberInRange(digits, 150, 193)
      case "in" => checkNumberInRange(digits, 59, 76)
      case _ => false

def validateColor(color: String): Boolean =
  color.length == 7 && color.matches("^#-?[0-9a-fA-F]+")

def passportClaimIsValid(claim: PassportClaim): Boolean = 
  claim match 
    case PassportClaim("cid", _) => true
    case PassportClaim("byr", year) => checkNumberInRange(year, 1920, 2002)
    case PassportClaim("iyr", year) => checkNumberInRange(year, 2010, 2020) 
    case PassportClaim("eyr", year) => checkNumberInRange(year, 2020, 2030) 
    case PassportClaim("hgt", height) => validateHeight(height)
    case PassportClaim("hcl", color) => validateColor(color)
    case PassportClaim("ecl", color) => eyeColors.contains(color)
    case PassportClaim("pid", passportId) => passportId.length == 9 && passportId.forall(_.isDigit)
    case _ => false