package controllers.service.rest

import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import scala.concurrent.Future
import reactivemongo.api._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import models.beans.IntroImage
import models.beans.JsonFormats._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.cache.Cached
import play.api.Play.current
import com.wordnik.swagger.annotations._
import com.wordnik.swagger.core.util.ScalaJsonUtil
import controllers.service.BaseApiController
import models.beans.EnumTableList.IMAGE

@Api(value = "/image", description = "All images information")
object ImageController extends BaseApiController {

  def collection: JSONCollection = db.collection[JSONCollection](IMAGE.toString())

  import play.api.data.Form
  
  @ApiOperation(
    nickname = "getIntroImage", 
    value = "List all frontpage images", 
    notes = "Returns all intro", 
    response = classOf[IntroImage],
    responseContainer = "List",
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 404, message = "Images not found")))
  def findAllIntroImage() = Cached("ImageCache") {Action.async {
    Logger.info("Started Image Cache")
    // let's do our query
    val cursor: Cursor[IntroImage] = collection.find(Json.obj()).cursor[IntroImage]

    val futureIntroImageList: Future[List[IntroImage]] = cursor.collect[List]()
	
    futureIntroImageList.map { introImage =>
      
      introImage.size match {
        case 0 => JsonResponse(NotFound(Json.obj()))
        case _ => JsonResponse(Ok(Json.obj("backgrounds" -> toIntroImageLowLevel(introImage))),3600)
      }
    }
  }}
  
  def index = Action { Ok("works") }
}