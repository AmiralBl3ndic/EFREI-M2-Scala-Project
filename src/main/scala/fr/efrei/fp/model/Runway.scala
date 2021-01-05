package fr.efrei.fp.model

import scala.io.Source
import scala.util.Try

case class Runway(
                   id: Int,
                   airport_id: Int,
                   ident: String,
                   length_ft: Int,
                   width_ft: Int,
                   surface: String,
                   lighted: Boolean,
                   closed: Boolean,
                   le_ident: String,
                   le_latitude_deg: Float,
                   le_longitude_deg: Float,
                   le_elevation_ft: Int,
                   le_heading_degT: Float,
                   le_displaced_threshold_ft: Int,
                   he_ident: String,
                   he_latitude_deg: Float,
                   he_longitude_deg: Float,
                   he_elevation_ft: Int,
                   he_heading_degT: Float,
                   he_displaced_threshold_ft: Int)

object Runway {
  def parseAllFromCSV(csvFilePath: String): Either[String, Array[Runway]] = {
    val csvFile = Source.fromFile(csvFilePath, "UTF-8")
    val countries = csvFile.getLines.drop(1).map(buildFromCSVLine)
    val toReturn = countries.filter(_.isRight).map(_.right.get).toArray
    csvFile.close

    if (toReturn.length == 0) {
      Left(s"Unable to parse any runway from $csvFilePath")
    } else {
      Right(toReturn)
    }
  }

  private def buildFromCSVLine(csvLine: String): Either[String, Runway] = {
    val csvBits = csvLine.split(",").map(el => {
      val e = el.trim
      if (e.startsWith(",") && e.endsWith(",")) {
        e.slice(1, e.length - 1)
      } else {
        e
      }
    })

    csvBits.length match {
      case x if x < 20 => Left(s"Unable to parse incomplete data, 20 columns expected, only $x found")
      case _ =>
        val idEither = Try(csvBits(0).toInt).toEither
        val airportIdEither = Try(csvBits(1).toInt).toEither
        val ident = csvBits(2)
        val lengthFtEither = Try(csvBits(3).toInt).toEither
        val widthFtEither = Try(csvBits(4).toInt).toEither
        val surface = csvBits(5)
        val lighted = Try(csvBits(6).toInt).getOrElse(0) == 1
        val closed = Try(csvBits(7).toInt).getOrElse(0) == 1
        val le_ident = csvBits(8)
        val le_latitude_degEither = Try(csvBits(9).toFloat).toEither
        val le_longitude_degEither = Try(csvBits(10).toFloat).toEither
        val le_elevation_ftEither = Try(csvBits(11).toInt).toEither
        val le_heading_degTEither = Try(csvBits(12).toFloat).toEither
        val le_displaced_threshold_ftEither = Try(csvBits(13).toInt).toEither
        val he_ident = csvBits(14)
        val he_latitude_degEither = Try(csvBits(15).toFloat).toEither
        val he_longitude_degEither = Try(csvBits(16).toFloat).toEither
        val he_elevation_ftEither = Try(csvBits(17).toInt).toEither
        val he_heading_degTEither = Try(csvBits(18).toFloat).toEither
        val he_displaced_threshold_ftEither = Try(csvBits(19).toInt).toEither

        val eithers = Array(
          idEither,
          airportIdEither,
          lengthFtEither,
          widthFtEither,
          le_latitude_degEither,
          le_longitude_degEither,
          le_elevation_ftEither,
          le_heading_degTEither,
          le_displaced_threshold_ftEither,
          he_latitude_degEither,
          he_longitude_degEither,
          he_elevation_ftEither,
          he_heading_degTEither,
          he_displaced_threshold_ftEither,
        )

        val floatEithers = Array(le_latitude_degEither, le_longitude_degEither, le_heading_degTEither,
          he_latitude_degEither, he_longitude_degEither, he_heading_degTEither)
        val intEithers = Array(idEither, airportIdEither, lengthFtEither, widthFtEither, le_elevation_ftEither,
          le_displaced_threshold_ftEither, he_elevation_ftEither, he_displaced_threshold_ftEither)

        if (eithers.exists(_.isLeft)) {
          Left("A column contains wrong data")
        } else {
          val intVals = intEithers.map(_.right.get)
          val floatVals = floatEithers.map(_.right.get)

          Right(Runway(
            id = intVals(0),
            airport_id = intVals(1),
            ident = ident,
            length_ft = intVals(2),
            width_ft = intVals(3),
            surface = surface,
            lighted = lighted,
            closed = closed,
            le_ident = le_ident,
            le_latitude_deg = floatVals(0),
            le_longitude_deg = floatVals(1),
            le_elevation_ft = intVals(4),
            le_heading_degT = floatVals(2),
            le_displaced_threshold_ft = intVals(5),
            he_ident = he_ident,
            he_latitude_deg = floatVals(3),
            he_longitude_deg = floatVals(4),
            he_elevation_ft = intVals(6),
            he_heading_degT = floatVals(5),
            he_displaced_threshold_ft = intVals(7)
          ))
        }
    }
  }
}