package models.beans

import play.api.libs.json._
import play.api.mvc.WebSocket.FrameFormatter

object WeatherModel {
  
  case class WeatherRequest(state:String, date:String)
  case class WeatherResponse(icon:String, summary:String, date:String)
  
  implicit val inWeatherFormat = Json.format[WeatherRequest]
  implicit val outWeatherFormat = Json.format[WeatherResponse]
  implicit val inWeatherFormatter = FrameFormatter.jsonFrame[WeatherRequest]
  implicit val outWeatherFormatter = FrameFormatter.jsonFrame[WeatherResponse]
  
  val location = Map(
      "KL" -> ("Kuala Lumpur", "3.1357", "101.6880"),
      "PT" -> ("Putrajaya", "2.9431", "101.6994"),
      "JB" -> ("Johor", "1.4872", "103.7811"),
      "PG" -> ("Penang", "5.400", "100.2333"),
      "KD" -> ("Kedah", "6.1283", "100.3628"),
      "KT" -> ("Kelantan", "5.2500", "102.0000"),
      "MC" -> ("Malacca", "2.2000", "102.2500"),
      "NS" -> ("Negeri Sembilan", "2.7500", "102.2500"),
      "PH" -> ("Pahang", "3.7500", "102.5000"),
      "PR" -> ("Perak", "4.7500", "101.0000"),
      "PL" -> ("Perlis", "6.5000", "100.2500"),
      "SB" -> ("Sabah", "5.2500", "117.0000"),
      "SW" -> ("Sarawak", "3.0381", "113.7811"),
      "TG" -> ("Terrengganu", "4.7500", "103.0000")
      ) withDefaultValue(("Kuala Lumpur", "3.1357" , "101.6880"))
}