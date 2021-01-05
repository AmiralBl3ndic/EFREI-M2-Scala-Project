package fr.efrei.fp.model.util

sealed trait Continent
object Continent {
	case object EU extends Continent
	case object AS extends Continent
	case object NA extends Continent
	case object AF extends Continent
	case object SA extends Continent
	case object OC extends Continent
	case object AN extends Continent

	def parse(s: String): Either[String, Continent] = s match {
		case "EU" => Right(Continent.EU)
		case "AS" => Right(Continent.AS)
		case "NA" => Right(Continent.NA)
		case "AF" => Right(Continent.AF)
		case "SA" => Right(Continent.SA)
		case "OC" => Right(Continent.OC)
		case "AN" => Right(Continent.AN)
		case _ => Left(s"""Unable to parse Continent from "$s"""")
	}
}
