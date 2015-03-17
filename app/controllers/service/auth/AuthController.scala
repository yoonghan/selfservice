package controllers.service.auth

import play.api._
import play.api.mvc._
import util.{Failure, Success}
import com.jaring.jom.util.authentication._
import reactivemongo.api._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import reactivemongo.core.commands.GetLastError
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import models.auth.OAuth2Authentication
import models.auth.OAuth2Authentication._
import controllers.jobs.LogActor
import scala.concurrent.Future
import utils.CommonKeys._
import models.beans.UserModel._
import controllers.service.BaseApiController
import models.auth.SecurityKey
import models.beans.EnumTableList.USER 

object AuthController extends BaseApiController with MongoController{
  
  def collection: JSONCollection = db.collection[JSONCollection](USER.toString())

  /**
   * Start building the oauth to send
   */
  def oauthSend(aType: String) = Action {
    val authFactory = OAuth2Authentication.getOAuth(aType)
    
    if(!authFactory.isEmpty){
      val authFac = authFactory.get
      val stateToken = filterTokenWithDevice(authFac.generateStateToken,aType)
      
      val url = authFac.getGeneratedOAuthURL(stateToken)
      
      if(url.isDefined){
        Redirect(url.get).flashing((TOKEN_KEY, stateToken))
        
      }else{
        BadRequest(views.html.help("Sorry, this page is just a servicing page."))
      }
    }else{
    	InternalServerError(views.html.help("We had some really bad configurations, sorry."))
    }
  }
  
  def filterTokenWithDevice(token:String, aType:String):String = {
    if(aType.startsWith(DEVICE_APP))
      DEVICE_APP +  token
    else
      token
  }
  
  /**
   * Start checking when oauth is received
   */
  def oauthReceive(authCode:String, state:String, error_code:String, error_message:String) = 
    Action.async{ request =>
    
    import scala.concurrent.duration._ 
     
    def checkUserToUpdate(userId:String, otype:String):Future[UserAccess] = {
      val cursor:Cursor[User] = collection.find(Json.obj("id" -> userId, "otype" -> otype, "newUser" -> false)).cursor[User]
      val futureUserList: Future[List[User]] = cursor.collect[List]()
      val futureVal = futureUserList.map { userList =>
		  userList.size match {
		    case 1 => UserAccess(true , userList(0).cpId.getOrElse(EMPTY_CPID), userList(0).authLevel.getOrElse(AUTH_DEFAULT_LVL))
		    case _ => UserAccess(false , EMPTY_CPID, AUTH_DEFAULT_LVL )
		  }
		}
      futureVal
    }
      
    def parseJSONToCreateUser(json:String, aType:String):Future[UserCheck] = {
      
      val otype = getBaseOType(aType);
      val jsonObj = Json.parse(json)
      val userId = jsonObj \ "id"
      val foundUser = checkUserToUpdate(userId.as[String],otype).map(
		    result =>{
				val con_jsonObj = jsonObj.as[JsObject] + ("otype" , JsString(otype)) + ("newUser" , JsBoolean(!result.found)) + ("authLevel", JsNumber(result.accessLvl)) + ("cpId", JsString(result.cpId))
									
				try{
				  collection.update(Json.obj("id"->userId , "otype"-> otype.toUpperCase()), con_jsonObj, GetLastError(), upsert = true, multi = false)
				  Logger.info("Updated user: " + userId)
				}catch{
				  case e:Exception => e.printStackTrace(); LogActor.logActor ! e.getMessage()
				  //if never got inserted it's ok
				}
				UserCheck(userId.as[String], result.cpId, result.accessLvl, result.found)
		    })
		foundUser
	}
	
	def hasCheckedError(error_code:String, error_message:String) = {
	  (error_code != "" && error_message != "")
	}
	
	//code here
	val sessState = request.flash(TOKEN_KEY)
	
	if( ! hasCheckedError(error_code, error_message)){
	    import controllers.service.auth.AuthActor
	    Authorize.authorize(state, authCode, sessState).flatMap{
	      case Success(result) =>{
	        val oType = result._2
	        val returnVal = parseJSONToCreateUser(result._1, oType)
	        returnVal.map(
        		value =>{
        		  val direction = if(value.found) REDIRECT_EXISTING_USER_URL else REDIRECT_NEW_USER_URL
        		  val userName = value.userId 
        		  val userId = userIDCombination(oType,userName,false)
        		  val key = SecurityKey.getRandKey
        		  val publicKey = SecurityKey.encode(userId, key)
        		  val cpId = value.cpId 
        		  val accessLvl = value.accessLvl
        		  
        		  insertPassIntoDB(userId, key, cpId, accessLvl); 
        		  
        		  Redirect(direction).withSession((USER_ID, userName),(OTYPE, oType),(PUBLIC_KEY, publicKey),(CP_ID, cpId),(ACCESS_LVL, String.valueOf(accessLvl)))
        		}
	        )
	      }
	      case Failure(e) => Future.successful(Unauthorized("Failed to connect:-"+e.getMessage()))
	    }
	}else{
	    //Apparently, This error only happens in facebook.
	    LogActor.logActor ! "Error Code:" + error_code + ",Error Message:" + error_message
	    Future.successful(InternalServerError(views.html.help("We had some really bad configurations, sorry.")))
	}

  }
  
  case class UserCheck(userId:String, cpId:String, accessLvl:Int, found:Boolean)
  case class UserAccess(found:Boolean, cpId:String, accessLvl:Int)
}