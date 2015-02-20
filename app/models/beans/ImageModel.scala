package models.beans

import play.api.libs.json._
import scala.math.BigDecimal.int2bigDecimal

case class IntroImage(
	src: String,
	desc: String
)

object JsonFormats {
  import play.api.libs.json.Json
  import play.api.data._
  import play.api.data.Forms._

  // Generates Writes and Reads for Feed and User thanks to Json Macros
  implicit val introImageFormat = Json.format[IntroImage]
  
  def toIntroImageLowLevel(introImages: List[IntroImage]) = {
	  for(image <- introImages) yield JsObject("src" -> JsString(image.src)::"fade" -> JsNumber(1000)::Nil)
  }
}