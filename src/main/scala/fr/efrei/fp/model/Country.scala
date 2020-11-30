package fr.efrei.fp.model

import fr.efrei.fp.util.Digit

case class Country(id: Array[Digit], code: String, name: String, continent: Continent, wikipedia_link: String, keywords: Array[String])



object Country {
  private def parseCountryId(countryId: String): Either[String, Array[Digit]] = {
    val countryIdDigits = countryId.toCharArray.map(Digit.buildFrom)

    // Check if format is valid
    if (countryIdDigits.foldLeft(false) { (acc, id) =>
      id match {
        case x if acc || x.isLeft => true
        case _ => false
      }
    }) {
      Left(s"Cannot parse non-digit characters in \"$countryId\"")
    } else {
      Right(countryIdDigits.map(_.right.get))
    }
  }

  def buildFrom(csvLine: String): Either[String, Country] = {
    val csvBits = csvLine.split(",").map(
      _.trim
    )

    csvBits.length match {
      case x if x < 6 => Left("Unable to parse incomplete data, 6 columns expected, only $x found")
      case _ => {
        // Parse id and ensure it is only made of digits
        val countryId = parseCountryId(csvBits(0))
        val countryCode = csvBits(1)
        val countryName = csvBits(2)
        val countryContinent = Continent.parse(csvBits(3))
        val countryWikiLink = csvBits(4)
        val countryKeywords = csvBits.drop(5)

        (countryId, countryContinent) match {
          case (Right(id), Right(continent)) => Right(Country(id, countryCode, countryName, continent, countryWikiLink, countryKeywords))
          case _ => Left("Unable to parse country fro some reason")
        }
      }
    }
  }
}
