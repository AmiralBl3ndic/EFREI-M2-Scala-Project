package fr.efrei.fp.model.util

sealed trait MenuChoice
object MenuChoice {
  case object COUNTRIES_WITH_MOST_AIRPORTS extends MenuChoice
  case object COUNTRIES_WITH_LESS_AIRPORTS extends MenuChoice
  case object TYPES_OF_RUNWAYS_PER_COUNTRY extends MenuChoice
  case object MOST_COMMON_RUNWAY_LATITUDES extends MenuChoice
  case object EXIT extends MenuChoice
  case object INVALID_CHOICE extends MenuChoice
}