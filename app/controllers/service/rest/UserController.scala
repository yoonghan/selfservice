package controllers.service.rest

import play.api._
import play.api.mvc._
import controllers.service.BaseApiController
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import models.beans.UserModel._
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
import models.beans.EnumTableList.{PROFILE,USER} 

@Api(value = "/user", description = "All user information")
object UserController extends BaseApiController with MongoController {
  
  def userCollection: JSONCollection = db.collection[JSONCollection](USER.toString())
  def profileCollection: JSONCollection = db.collection[JSONCollection](PROFILE.toString())
  
  /**
   * Created to get user information
   */
  @ApiOperation(
    nickname = "getUserBasicInformation", 
    value = "User name", 
    notes = "Returns user name", 
    response = classOf[User],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def userInfo = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
    def splitName(name:String, newUser:Boolean) = {
      val splittedName = name.split(" ");
      if(splittedName.length > 0){
        UserName(splittedName(0), splittedName(1), newUser)
      }else{
        UserName(splittedName(0), "", newUser)
      }
    }
    
	val userId = request.session(USER_ID)
	val oType = request.session(OTYPE)

	val cursor:Cursor[User] = userCollection.find(Json.obj("id" -> userId, "otype" -> oType)).cursor[User]
	val futureUserList: Future[List[User]] = cursor.collect[List]()
	
    val futureVal = futureUserList.map { userList =>
      userList.size match {
        case 0 => JsonResponse(NotFound(Json.obj()))
        case _ => {
          
          val userName = splitName(userList(0).name, userList(0).newUser )
          JsonResponse(Ok(Json.toJson(userName)))
        }
      }
    }
	
	futureVal
  }
  
  /**
   * Check current user access
   */
  @ApiOperation(
    nickname = "getUserAccesses", 
    value = "List of user allowed accesses", 
    notes = "Returns the list of allowed accesses. 0 as default, 2 as with calendar setup authority.", 
    response = classOf[User],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def accesses = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
    
	val userId = request.session(USER_ID)
	val oType = request.session(OTYPE)

	val cursor:Cursor[User] = userCollection.find(Json.obj("id" -> userId, "otype" -> oType)).cursor[User]
	val futureProfileList: Future[List[User]] = cursor.collect[List]()
	
    futureProfileList.map { userList =>
      userList.size match {
        case 0 => JsonResponse(NotFound(Json.obj()))
        case _ => {
          JsonResponse(Ok(Json.toJson(userList(0).authLevel)))
        }
      }
    }
  }
  
  /**
   * Get user information
   */
  @ApiOperation(
    nickname = "getProfileBasicInformation", 
    value = "Basic user profile", 
    notes = "Returns basic user profile, including name, contact, state", 
    response = classOf[UserProfile],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def profile = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
    
	val userId = request.session(USER_ID)
	val oType = request.session(OTYPE)

	val cursor:Cursor[UserProfile] = profileCollection.find(Json.obj("_id" -> userIDCombination(oType,userId))).cursor[UserProfile]
	val futureProfileList: Future[List[UserProfile]] = cursor.collect[List]()
	
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
   * Insert profile
   */
  @ApiOperation(
    nickname = "insertNewUserProfile", 
    value = "Create a new user profiles", 
    notes = "Create a new user only if it does not exist, else updates it. Only facebook and gmail is available.", 
    response = classOf[String],
    httpMethod = "PUT"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def profileIns = AuthorizeUser(BodyParsers.parse.json){request =>
    
    import reactivemongo.core.commands.GetLastError
    
    val userProfile = request.body.validate[UserProfile];
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    
    userProfile.fold(
        errors => {
          JsonResponse(BadRequest(Json.obj("error"->"Unexpected Request, what have you sent?")));
        },
        profile => {
          val errorList = validateInput(profile)
          if(errorList.isEmpty){
            
            val userId = request.session(USER_ID)
            val oType = request.session(OTYPE)
            val jsonObj = Json.toJson(profile)
            
            val ins_jsonObj = jsonObj.as[JsObject] + ("_id" -> JsString( userIDCombination(oType , userId) ))
            val con_jsonObj = Json.obj( "$set" -> Json.obj( "newUser" -> JsBoolean(false)))
            Logger.info("USER_ID: "+userIDCombination(oType , userId))
            profileCollection.insert(ins_jsonObj)
            userCollection.update(Json.obj("id"->userId , "otype"-> oType.toUpperCase()), con_jsonObj, GetLastError(), upsert = false, multi = false)
        	JsonResponse(Created(Json.obj("success"->"OK")))
          }else{
            JsonResponse(BadRequest(toUserError(errorList)));
          }
        }
      )
  }
  
  /**
   * Update profile
   */
  @ApiOperation(
    nickname = "updateUserProfile", 
    value = "Update user profile", 
    notes = "Update user profile, user profile must be created before hand.", 
    response = classOf[String],
    httpMethod = "PUT"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def profileUpd = AuthorizeUser(BodyParsers.parse.json){request =>
    
    import reactivemongo.core.commands.GetLastError
    
    val userProfile = request.body.validate[UserProfile];
    
    userProfile.fold(
        errors => {
          JsonResponse(BadRequest(Json.obj("error"->"Unexpected Request, what have you sent?")));
        },
        userProfile => {
          val errorList = validateInput(userProfile)
          if(errorList.isEmpty){
            
            val userId = request.session(USER_ID)
            val oType = request.session(OTYPE)
            val jsonObj = Json.toJson(userProfile)
            
            val userAuthId = Json.obj("_id" -> JsString( userIDCombination(oType , userId) ))
            
            profileCollection.update(userAuthId, jsonObj, GetLastError(), upsert = false, multi = false)
        	JsonResponse(Created(Json.obj("success"->"OK")))
          }else{
            JsonResponse(BadRequest(toUserError(errorList)));
          }
        }
      )
  }

  
  def logout = AuthorizeUser(BodyParsers.parse.anyContent){request =>
    
    import reactivemongo.core.commands.GetLastError
    
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val userAuth = userIDCombination(oType, userId)
    
    removePassFromDB(userAuth);
    
    Redirect(LOGOUT_LOCATION);
  }

  /**
   * This method is only available for testing as oauth is only workable for production
   * 1. Updates the records.
   * 2. Send the records as a cookie.
  */
  def testprofile = Action {
    import reactivemongo.core.commands.GetLastError;
	
    val value:Option[String] = current.configuration.getString("environment")
    if(value.isDefined && value.get == "test"){
      
      val setUSERID="114852108498604797792";
      val setTYPE="G";
      val userId = userIDCombination(setTYPE, setUSERID)
      val accessLvl = AUTH_CAL_CREATE + DEFAULT_AUTH_LVL
      val cpId = "54a4a4341d86e330ad740088";
      
    	val con_jsonObj = Json.obj( "$set" -> Json.obj( "newUser" -> JsBoolean(true), "authLevel" -> accessLvl, "cpId" -> cpId))
    	userCollection.update(Json.obj("id"->setUSERID , "otype"-> setTYPE), con_jsonObj, GetLastError(), upsert = false, multi = false)
    	
    	val key = SecurityKey.getRandKey
    	val publicKey = SecurityKey.encode(userId, key)
    	
    	insertPassIntoDB(userId, key, cpId, accessLvl);
    	
    	Redirect(routes.UserController.userInfo()).
	        withSession((USER_ID, setUSERID),(OTYPE, setTYPE),(PUBLIC_KEY, publicKey),(CP_ID, cpId),(ACCESS_LVL, String.valueOf(accessLvl)))
    }else
	  Unauthorized("You are not allowed to use this service.")
  }
  
  
}