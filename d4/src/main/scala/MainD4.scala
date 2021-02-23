import scala.util.chaining._

@main def entrypoint = 
  println(solve(FileLoader.readFile("input.txt")))

val mandatoryFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

def solve(input: List[String]): Int = 
  splitInputToRawPassports(input).map(parsePassportElements).count(validatePassportClaims)
  
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

def validatePassportClaims(claims: List[PassportClaim]): Boolean = 
  claims.forall(claim => claim.name == "cid" || mandatoryFields.contains(claim.name))
  .tap(res => println(s"${claims.map(_.name) mkString " "} :: res: $res"))