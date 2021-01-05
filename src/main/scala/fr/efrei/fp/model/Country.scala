package fr.efrei.fp.model

import fr.efrei.fp.model.util.{Continent, CountryCode, Digit}

import java.net.URL
import scala.io.Source
import scala.util.Try

case class Country(id: Array[Digit], code: CountryCode, name: String, continent: Continent, wikipedia_link: URL, keywords: Array[String])

object Country {

  def parseAllFromCSV(csvFilePath: String): Either[String, Array[Country]] = {
    val csvFile = Source.fromFile(csvFilePath, "UTF-8")
    val countries = csvFile.getLines.drop(1).map(buildFromCSVLine)

    if (!countries.exists(_.isRight)) {
      csvFile.close
      Left(s"Unable to parse any country from $csvFilePath")
    } else {
      val toReturn = countries.filter(_.isRight).map(_.right.get).toArray
      csvFile.close
      Right(toReturn)
    }
  }

  def buildFromCSVLine(csvLine: String): Either[String, Country] = {
    val csvBits = csvLine.split(",").map(e => {
      val element = e.trim
      if (element.startsWith("\"") && element.endsWith("\""))
        element.slice(1, element.length - 1)
      else
        element
    })

    csvBits.length match {
      case x if x < 6 => Left(s"Unable to parse incomplete data, 6 columns expected, only $x found")
      case _ =>
        // Parse id and ensure it is only made of digits
        val idEither = parseId(csvBits(0))
        val codeEither = CountryCode.parse(csvBits(1))
        val name = csvBits(2)
        val continentEither = Continent.parse(csvBits(3))
        val wikiLinkEither = Try(new URL(csvBits(4))).toEither
        val keywords = csvBits.drop(5)

        (idEither, codeEither, continentEither, wikiLinkEither) match {
          case (Left(x), _, _, _) => Left(x)
          case (_, Left(x), _, _) => Left(x)
          case (_, _, Left(x), _) => Left(x)
          case (_, _, _, Left(_)) => Left("Unable to parse wikipedia link")
          case (Right(id), Right(code), Right(continent), Right(wikiLink)) => Right(Country(id, code, name, continent, wikiLink, keywords))
          case _ => Left("Unable to parse country for some reason")
        }
    }
  }

  private def parseId(countryId: String): Either[String, Array[Digit]] = {
    val countryIdDigits = countryId.toCharArray.map(Digit.buildFrom)

    // Check if format is valid
    if (countryIdDigits.foldLeft(false) { (acc, id) =>
      id match {
        case x if acc || x.isLeft => true
        case _ => false
      }
    }) {
      Left(s"""Cannot parse non-digit characters in "$countryId"""")
    } else {
      Right(countryIdDigits.map(_.right.get))
    }
  }
}