import scala.util.chaining._
import scala.annotation.tailrec

@main def entrypoint = 
  println(solve(FileLoader.readFile("input.txt")))

val mandatoryFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

def solve(input: List[String]): Int = 
  splitInputToRawPassports(input)
    .map(parsePassportElements)
    // .tap(claims => claims.foreach(claim => println(s"${claim mkString " "} :: ${passportClaimsAreValid(claim)}")))
    .count(passportClaimsAreValid)
  
@tailrec
def splitInputToRawPassports(rows: List[String], passports: List[String] = List.empty): List[String] = 
    if rows.isEmpty then passports.toList
    else 
      val current = rows.takeWhile(l => l.nonEmpty) 
      splitInputToRawPassports(rows.drop(current.size + 1), passports :+ (current mkString " "))

case class PassportClaim(val name: String, val value: String)

def parsePassportElements(rawPassport: String): List[PassportClaim] =
  rawPassport.split(" ").toList.flatMap { claimValue => 
    claimValue.split(":").toList match 
      case claim :: value :: Nil => Some(PassportClaim(claim, value))
      case _ => None
    }

def passportClaimsAreValid(claims: List[PassportClaim]): Boolean = 
  mandatoryFields.diff(claims.map(_.name).toSet).toList match 
    case Nil => true
    case "cit" :: Nil => true
    case _ => false