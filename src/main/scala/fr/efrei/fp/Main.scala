package fr.efrei.fp

import fr.efrei.fp.model.util.{CountryCode, Digit, MenuChoice}
import fr.efrei.fp.model.{Airport, Country, Runway}

import java.util.Scanner
import scala.annotation.tailrec

object Main extends App {
  println("====================")

  println("Parsing CSV files...")

  println("\tParsing countries...")
	parseValidCountries() match {
    case Left(x) => println(x)
    case Right(countries) =>
      println("\tDone parsing and gathering parsable countries from countries.csv")

      println("\tParsing airports...")
      parseValidAirports() match {
        case Left(x) => println(x)
        case Right(airports) =>
          println("\tDone parsing and gathering parsable airports from airports.csv")

          println("\tParsing runways")
          parseValidRunways() match {
            case Left(x) => println(x)
            case Right(runways) =>
              println("\tDone parsing and gathering parsable runways from runways.csv")

              println("Done parsing CSV files\n====================\n")

              // Now we can actually do something
              doWorkForAsLongAsNeeded(countries, airports, runways)
          }
      }
  }

  def parseValidCountries() = Country.parseAllFromCSV("./src/resources/countries.csv")
  def parseValidAirports() = Airport.parseAllFromCSV("./src/resources/airports.csv")
  def parseValidRunways() = Runway.parseAllFromCSV("./src/resources/runways.csv")

  def showMenu(): Unit = {
    println("===== What do you want to do? =====")
    println("1. Get the 10 countries with highest number of airports (with count)")
    println("2. Get the 10 countries with lowest number of airports (with count)")
    println("3. Get the type or runways surfaces per country")
    println("4. Get the 10 most common runway latitudes")
    println("5. Do nothing and exit")
  }

  def askMenuChoice(): MenuChoice = {
    showMenu()

    val input = new Scanner(System.in).nextLine().replaceAll("\n", "").trim

    input match {
      case "1" => MenuChoice.COUNTRIES_WITH_MOST_AIRPORTS
      case "2" => MenuChoice.COUNTRIES_WITH_LESS_AIRPORTS
      case "3" => MenuChoice.TYPES_OF_RUNWAYS_PER_COUNTRY
      case "4" => MenuChoice.MOST_COMMON_RUNWAY_LATITUDES
      case "5" => MenuChoice.EXIT
      case _ => MenuChoice.INVALID_CHOICE
    }
  }

  @tailrec
  def validMenuChoice: MenuChoice = {
    val choice = askMenuChoice()
    if (choice == MenuChoice.INVALID_CHOICE) {
      println("Invalid choice, please try again\n")
      validMenuChoice
    } else {
      choice
    }
  }

  @tailrec
  def doWorkForAsLongAsNeeded(countries: Array[Country], airports: Array[Airport], runways: Array[Runway]): Unit = {
    validMenuChoice match {
      case MenuChoice.COUNTRIES_WITH_MOST_AIRPORTS =>
        println("\nThe top 10 countries with the most airports are:")
        top10CountriesWithMostAirports(countries, airports)
          .zipWithIndex
          .foreach(x => println(s"${x._2 + 1} - ${x._1._1.name} with ${x._1._2} airport${if (x._1._2 > 1) "s" else ""}"))

        println("\n")
        doWorkForAsLongAsNeeded(countries, airports, runways)

      case MenuChoice.COUNTRIES_WITH_LESS_AIRPORTS =>
        println("\nThe top 10 countries with the lesser airports are:")
        top10CountriesWithLessAirports(countries, airports)
          .zipWithIndex
          .foreach(x => println(s"${x._2 + 1} - ${x._1._1.name} with ${x._1._2} airport${if (x._1._2 > 1) "s" else ""}"))

        println("\n")
        doWorkForAsLongAsNeeded(countries, airports, runways)

      case MenuChoice.TYPES_OF_RUNWAYS_PER_COUNTRY =>
        println("\nTypes of runway surfaces in each country (only processing countries which runways were found for):")

        typesOfRunwaysPerCountry(countries, airports, runways)
          .sortBy(_._1.name)
          .foreach(x => {
            println(s"\t${x._1.name.capitalize}:")

            if (x._2.nonEmpty)
              x._2.foreach(surface => println(s"\t\t- $surface"))
            else
              println("\t\tNo surface data found for runways in that country")
          })

        println("\n")
        doWorkForAsLongAsNeeded(countries, airports, runways)

      case MenuChoice.MOST_COMMON_RUNWAY_LATITUDES =>
        println("\nThe top 10 most common runway latitudes are:")
        top10MostCommonRunwayLatitudes(runways).zipWithIndex.foreach(r => println(s"${r._2 + 1} - ${r._1._1}deg (${r._1._2} occurrences)"))

        println("\n")
        doWorkForAsLongAsNeeded(countries, airports, runways)

      case MenuChoice.EXIT => Unit
    }
  }

  def countryWithCode(countries: Array[Country], code: String): Option[Country] = countries.find(c => c.code.toString.equals(code))
  def countryWithCode(countries: Array[Country], code: CountryCode): Option[Country] = countryWithCode(countries, code.toString)

  def countriesWithAirports(countries: Array[Country], airports: Array[Airport]): Array[(Country, Array[Airport])] = {
    airports
      .filter(a => countryWithCode(countries, a.iso_country).isDefined)
      .groupBy(_.iso_country.toString)
      .map(x => (countryWithCode(countries, x._1).get, x._2))
      .toArray
  }

  def countriesSortedByAirportsCount(countries: Array[Country], airports: Array[Airport]): Array[(Country, Int)] = {
    countriesWithAirports(countries, airports)
      .map(x => (x._1, x._2.length))
      .filter(_._2 > 0)
      .sortBy(_._2)
  }

  def top10CountriesWithMostAirports(countries: Array[Country], airports: Array[Airport]): Array[(Country, Int)] =
    countriesSortedByAirportsCount(countries, airports).reverse.take(10)

  def top10CountriesWithLessAirports(countries: Array[Country], airports: Array[Airport]): Array[(Country, Int)] =
    countriesSortedByAirportsCount(countries, airports).take(10)

  def typesOfRunwaysPerCountry(countries: Array[Country], airports: Array[Airport], runways: Array[Runway]): Array[(Country, Array[String])] = {
    countriesWithAirports(countries, airports)
      .map(x => (x._1, x._2.map(a => Digit.parseInt(a.id))))
      .map(x => (x._1, x._2.flatMap(airportId => runwaysForAirportId(airportId, runways))))
      .map(x => (x._1, x._2.map(_.surface).distinct))
  }

  def runwaysForAirportId(airportId: Int, runways: Array[Runway]) = runways.filter(_.airport_id == airportId)

  def top10MostCommonRunwayLatitudes(runways: Array[Runway]): Array[(Float, Int)] = {
    runways
      .map(r => (r.le_latitude_deg, 1))
      .groupBy(_._1)
      .map(x => (x._1, x._2.length))
      .toArray
      .filter(_._2 > 0)
      .sortBy(_._2)
      .reverse
      .take(10)
  }
}