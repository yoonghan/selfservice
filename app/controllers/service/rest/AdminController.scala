package controllers.service.rest

import play.api._
import play.api.mvc._
import controllers.service.BaseApiController
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import models.beans.SubscriptionModel._
import play.api.Play.current
import play.api.libs.functional.syntax._
import scala.concurrent.Future
import reactivemongo.api._
import controllers.service.CommonKeys._
import controllers.service.ConfigurationSetup._
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.annotations.ApiOperation
import com.wordnik.swagger.annotations.ApiResponses
import com.wordnik.swagger.annotations.ApiResponse
import scala.concurrent.duration._
import play.api.data.Forms._
import models.auth.SecurityKey
import models.beans.EnumTableList.{SUBSCRIPTION}

@Api(value = "/admin", description = "All administrative controls")
object AdminController extends BaseApiController {

  def cpCollection: JSONCollection = db.collection[JSONCollection](SUBSCRIPTION.toString())
  
  /**
   * Get content provider information
   */
  @ApiOperation(
    nickname = "getContentProviderInformation", 
    value = "Basic content provider profile", 
    notes = "Returns CP profile", 
    response = classOf[Subscription],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def profile = AuthorizeAsyncUser(BodyParsers.parse.anyContent, AUTH_CAL_CREATE_LVL){request =>    
    
    val cpId = request.session(CP_ID)
    
    val query = Json.obj("_id" -> Json.obj("$oid"->cpId))
	val cursor:Cursor[Subscription] = cpCollection.find(query).cursor[Subscription]
	val futureProfileList: Future[List[Subscription]] = cursor.collect[List]()
	
    futureProfileList.map { profileList =>
      profileList.size match {
        case 0 => JsonResponse(NotFound(Json.obj()))
        case _ => {
          JsonResponse(Ok(Json.toJson(profileList(0))))
        }
      }
    }
  }

  /**
   * Update profile
   */
  @ApiOperation(
    nickname = "updateContentProviderProfile", 
    value = "Update content provider profile", 
    notes = "Update content provider profile, content provider profile must be created before hand.", 
    response = classOf[String],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def profileUpd = AuthorizeAsyncUser(BodyParsers.parse.json, AUTH_CAL_CREATE_LVL){request =>
    
    import reactivemongo.core.commands.GetLastError
    
    val cpProfile = request.body.validate[Subscription_Edit];
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val cpId = request.session(CP_ID) 
    
    cpProfile.fold(
        errors => {
          Future.successful(JsonResponse(BadRequest(Json.obj("error"->"Unexpected Request, what have you sent?"))));
        },
        cpProfile => {
          val errorList = validateSubscription_Edit(cpProfile)
          if(errorList.isEmpty){
            val jsonObj = Json.toJson(cpProfile)
            val modifiedObj = jsonObj.as[JsObject].-("ver")
            val cpId = request.session(CP_ID)
            
            val query = Json.obj("ver" -> cpProfile.ver, "_id" -> Json.obj("$oid"->cpId))
            val updateQuery = Json.obj("$set" -> modifiedObj, "$inc" -> Json.obj( "ver" -> 1))
            val futureUpdate = cpCollection.update(query, updateQuery, GetLastError(), upsert = false, multi = false)
            futureUpdate.map{ lastError =>
              if(lastError.updated == 1)
                JsonResponse(Created(Json.obj("success"->"OK")))
              else
                JsonResponse(NotFound(Json.obj("errors"->Json.arr("Someone has done a concurrent modification on this profile. Please reset to retrieve latest profile."))))
            }
          }else{
            Future.successful(JsonResponse(BadRequest(toSubscriptionError(errorList))));
          }
        }
      )
  }

}