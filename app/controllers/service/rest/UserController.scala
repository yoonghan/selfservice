package controllers.service.rest

import play.api._
import play.api.mvc._
import controllers.service.BaseApiController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import models.beans.UserModel._
import play.api.Play.current
import utils.Utility
import scala.concurrent.Future
import reactivemongo.api._
import utils.CommonKeys._
import utils.ConfigurationSetup._
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.annotations.ApiOperation
import com.wordnik.swagger.annotations.ApiResponses
import com.wordnik.swagger.annotations.ApiResponse
import models.auth.SecurityKey
import reactivemongo.core.commands.GetLastError;
import models.beans.EnumTableList.{PROFILE,USER} 

@Api(value = "/user", description = "All user information")
object UserController extends BaseApiController {
  
  def userCollection: JSONCollection = db.collection[JSONCollection](USER.toString())
  def profileCollection: JSONCollection = db.collection[JSONCollection](PROFILE.toString())
  
  val COMMON_SEPERATOR = "|"	//ensure encoding does not have |, duh!
  
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
  def userInfo = AuthorizeAsyncUser(BodyParsers.parse.empty){request =>
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
        case 0 => JsonResponse(NotFound(ERR_COMMON_NO_RECORD_FOUND))
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
  def accesses = AuthorizeAsyncUser(BodyParsers.parse.empty){request =>
    
	val userId = request.session(USER_ID)
	val oType = request.session(OTYPE)

	val cursor:Cursor[User] = userCollection.find(Json.obj("id" -> userId, "otype" -> oType)).cursor[User]
	val futureProfileList: Future[List[User]] = cursor.collect[List]()
	
    futureProfileList.map { userList =>
      userList.size match {
        case 0 => JsonResponse(NotFound(ERR_COMMON_NO_RECORD_FOUND))
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
  def profile = AuthorizeAsyncUser(BodyParsers.parse.empty){request =>
    
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)

    val cursor:Cursor[UserProfile] = profileCollection.find(Json.obj("_id" -> userIDCombination(oType,userId))).cursor[UserProfile]
    val futureProfileList: Future[List[UserProfile]] = cursor.collect[List]()

    futureProfileList.map { profileList =>
      profileList.size match {
        case 0 => JsonResponse(NotFound(ERR_COMMON_NO_RECORD_FOUND))
        case _ => {
          JsonResponse(Ok(Json.toJson(profileList(0))))
        }
      }
    }
  }

  @ApiOperation(
  nickname = "getUserCodeForRegistration",
  value = "Get User Code for Registration",
  notes = "Returns basic user fit for registration",
  response = classOf[Byte],
  httpMethod = "GET"
  )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def getRegistrationNo = AuthorizeAsyncUser(BodyParsers.parse.empty){request =>
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val width = "600"
    val height = "50"
    val query = Json.obj("id" -> userId, "otype" -> oType)

    val cursor:Cursor[UserConfidential] = userCollection.find(query).cursor[UserConfidential]
    val futureUserList:Future[List[UserConfidential]] = cursor.collect[List]()
    futureUserList.map{ userList =>
      userList.size match{
          case 1 =>{
            val encrypt_id = hashCodeUserLogin(oType=oType, userList(0)._id)

            JsonResponse(ImageResponse(".gif",Utility.getImageURL(encrypt_id, width=width, height=height)))
          }
          case _ =>
            JsonResponse(ImageResponse(".gif",Utility.getImageURL("No id found. Are you logged in?", width=width, height=height)))
      }
    }
  }

  @ApiOperation(
  nickname = "addUserIdsForRegistration",
  value = "Add User for Registration",
  notes = "Register user",
  response = classOf[String],
  httpMethod = "PUT"
  )
  def addUserAsProvider(userCode:String) = AuthorizeAsyncUser(BodyParsers.parse.empty, AUTH_CAL_CREATE_LVL){request =>
    val cpId = request.session(CP_ID)

    val (oType, realId) = unhashCodeUserLogin(userCode)

    if(oType == " " || realId == " " || !realId.matches(MONGODB_HEX_ID_PATTERN)){
      Future.successful(JsonResponse(NotFound(ERR_COMMON_NO_RECORD_FOUND)))
    }else {

      val userQuery = Json.obj(
        "_id" -> Json.obj("$oid" -> realId),
        "otype" -> oType,
        "newUser" -> false)

      val cursor: Cursor[User] = userCollection.find(userQuery).cursor[User]
      val futureUserList: Future[List[User]] = cursor.collect[List]()

      futureUserList.flatMap { userList =>
        userList.size match {
          case 1 => {
            val q_userQuery = userQuery ++ Json.obj("cpId" -> Json.obj("$ne" -> cpId))
            val updateVal = Json.obj("$set" -> Json.obj("cpId" -> cpId, "authLevel" -> (AUTH_CAL_CREATE_LVL | userList(0).authLevel.getOrElse(AUTH_DEFAULT_LVL))))
            val futureUpdateUser = userCollection.update(q_userQuery, updateVal, GetLastError(), false, false)
            futureUpdateUser.map {
              status =>
                if (status.updated == 1) {
                  JsonResponse(Ok(SUC_COMMON_OK))
                } else {
                  JsonResponse(NotFound(JSON_KEYWORD_ERRORS("Current user has subscribed to a CP")))
                }
            }
          }
          case _ => Future.successful(JsonResponse(NotFound(ERR_COMMON_NO_RECORD_FOUND)))
        }
      }
    }
  }

  @ApiOperation(
    nickname = "removeUserFromRegistration",
    value = "Remove User from Registration",
    notes = "Returns basic user fit for registration",
    response = classOf[String],
    httpMethod = "DELETE"
  )
  def removeUserFromRegistration(userCode:String) = AuthorizeAsyncUser(BodyParsers.parse.empty, AUTH_CAL_CREATE_LVL){request =>
    val cpId = request.session(CP_ID)
    val userId = request.session(USER_ID)
    val otype = request.session(OTYPE)

    val (oType, realId) = unhashCodeUserLogin(userCode)

    val query = Json.obj(
      "_id" -> Json.obj("$oid" -> realId),
      "otype" -> oType)
    val cursor: Cursor[User] = userCollection.find(query).cursor[User]
    val futureUserList: Future[List[User]] = cursor.collect[List]()
    futureUserList.flatMap { userList =>
      userList.size match {
        case 1 => {
          val currUser = userList(0)
          if(currUser.id == userId && currUser.otype == otype) {
            Future.successful(JsonResponse(BadRequest(JSON_KEYWORD_ERRORS("Not allowed to remove self"))))
          }else{
            val authority = (currUser.authLevel).getOrElse(AUTH_DEFAULT_LVL)
            val newAuthLevel = if ((authority & AUTH_CAL_CREATE_LVL) == AUTH_CAL_CREATE_LVL) (authority - AUTH_CAL_CREATE_LVL) else authority
            val updateVal = Json.obj("$unset" -> Json.obj("cpId" -> ""), "$set" -> Json.obj("authLevel" -> newAuthLevel))
            val futureUpdateUser = userCollection.update(query, updateVal, GetLastError(), false, false)
            futureUpdateUser.map {
              status =>
                if (status.updated == 1) {
                  JsonResponse(Ok(SUC_COMMON_OK))
                } else {
                  JsonResponse(NotFound(JSON_KEYWORD_ERRORS("User may have been removed by another admin")))
                }
            }
          }
        }
        case _ => Future.successful(JsonResponse(NotFound(JSON_KEYWORD_ERRORS("User not found."))))
      }
    }
  }

  def hashCodeUserLogin(oType:String, realId:String):String = {
    val fullCode = oType + COMMON_SEPERATOR + realId  + COMMON_SEPERATOR +  "E"
    com.google.api.client.util.Base64.encodeBase64URLSafeString(fullCode.getBytes())
  }

  def unhashCodeUserLogin(userCode:String):(String, String) = {
    val decode = try {
        new String(com.google.api.client.util.Base64.decodeBase64(userCode))
      }catch{
        case e:Exception => " | "
      }
    val validRegex = """[0-9|a-z|A-Z|\|]+"""
    val validWord = if(decode.matches(validRegex)) decode else " | "
    val splitVal = validWord.split("""\|""")
    (splitVal(0), splitVal(1))
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
    
    userProfile.fold(
      errors => {
        JsonResponse(BadRequest(JSON_KEYWORD_ERRORS("Unexpected Request, what have you sent?")));
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
          JsonResponse(Created(SUC_COMMON_OK))
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
          JsonResponse(Created(SUC_COMMON_OK))
        }else{
          JsonResponse(BadRequest(toUserError(errorList)));
        }
      }
    )
  }

  /**
   * Retrieve subscription managers
   */
  @ApiOperation(
    nickname = "retrieveSubscriptionManager",
    value = "Retrieve subscription managers",
    notes = "Retrieve Subscription of the admin.",
    response = classOf[ContentProviderViewProfile],
    responseContainer = "List",
    httpMethod = "GET"
  )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def subscribedManagers = AuthorizeAsyncUser(BodyParsers.parse.empty, AUTH_CAL_CREATE_LVL){request =>
    val cpId = request.session(CP_ID)
    val query = Json.obj("cpId" -> cpId, "newUser" -> false)

    val cursor: Cursor[UserConfidential] = userCollection.find(query).cursor[UserConfidential]
    val futureUserReg: Future[List[UserConfidential]] = cursor.collect[List]()

    futureUserReg.flatMap { userList =>
      userList.size match {
        case 0 => Future.successful(JsonResponse(NotFound(ERR_COMMON_NO_RECORD_FOUND)))
        case _ => {
          val listOfUsers = userList.map(user => userIDCombination(user.otype, user.id))
          val query = Json.obj("_id" -> Json.obj("$in" -> listOfUsers))
          val cursor: Cursor[UserProfileWithId] = profileCollection.find(query).cursor[UserProfileWithId]
          val futureUserProfile: Future[List[UserProfileWithId]] = cursor.collect[List]()
          futureUserProfile.map { profileList =>
            profileList.size match {
              case 0 => JsonResponse(NotFound(ERR_COMMON_NO_RECORD_FOUND))
              case _ => {
                val masked = profileList.map( profile =>{
                  val realUserId: List[UserConfidential] = userList.filter(userModel => (userIDCombination(userModel.otype,userModel.id) == profile._id))//will return only 1 user

                  ContentProviderViewProfile(
                    hashCodeUserLogin(realUserId(0).otype, realUserId(0)._id),
                    profile.firstName,
                    profile.midName,
                    profile.lastName,
                    profile.gender,
                    profile.email,
                    profile.ctcNo
                    )
                })
                JsonResponse(Ok(Json.toJson(masked)))
              }
            }
          }
        }
      }
    }
  }

  
  def logout = AuthorizeUser(BodyParsers.parse.empty){request =>
    
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
  def testprofile(code:Int) = Action {

    val _userid = "114852108498604797792"
    val _name = "Test Data"
    val _type = "G"
    val _accessLvl = AUTH_CAL_CREATE_LVL + AUTH_DEFAULT_LVL
    val _cpId = "550374462c4125ab0049afc6"
    val _newUser = false

    code match {

      case 1 => { //user with all access
        runMe(_userid, _name, _type, _accessLvl, Option(_cpId), _newUser)
      }

      case 2 => { //user with new access
        val _type="F"
        val _cpId = Option("5512f9dd2c41254c05e11dcb")
        val _accessLvl = AUTH_DEFAULT_LVL + AUTH_CAL_CREATE_LVL
        val _newUser = false
        val _name = "Facebook Test"
        runMe(_userid, _name, _type, _accessLvl, _cpId, _newUser)
      }

      case 3 => {//user that is not registered as admin
        val _userid="114852108498604797792"
        val _type="F"
        val _accessLvl = AUTH_DEFAULT_LVL
        val _cpId = Option.empty
        val _newUser = false
        runMe(_userid, _name, _type, _accessLvl, _cpId, _newUser)
      }

      case 4=>{
        for(counter <- 1 to 1000) {
          val _userid = "1148521084"+counter
          val _type = "F"
          val _accessLvl = AUTH_CAL_CREATE_LVL + AUTH_DEFAULT_LVL
          val _cpId = Option("550374462c4125ab0049afc6")
          val _newUser = false
          runMe(_userid, _name, _type, _accessLvl, _cpId, _newUser)

          val opt = UserProfileWithId(
            _type+_userid,
            "Test",
            Option.empty,
            "Test Last",
            "M",
            "MY",
            Option.empty,
            Option.empty,
            Option.empty,
            Option.empty,
            "KL"
          )
          profileCollection.insert(opt)
        }
        Ok("Done")
      }
    }
  }

  private def runMe(user_id:String, name:String, o_type:String, access_Lvl:Int, cp_Id:Option[String], new_User:Boolean):Result = {
    val value:Option[String] = current.configuration.getString("environment")
    if(value.isDefined && value.get == "test"){
      val id = userIDCombination(o_type, user_id)
      val update = Json.obj( "newUser" -> JsBoolean(new_User), "authLevel" -> access_Lvl, "name" -> name)
      val con_jsonObj = if(cp_Id.isDefined){
                Json.obj("$set" -> update.++(Json.obj("cpId"->cp_Id.get)))
              } else Json.obj("$set" -> update).++(Json.obj("$unset"->Json.obj("cpId"->"")))

    	userCollection.update(Json.obj("id"->user_id , "otype"-> o_type), con_jsonObj, GetLastError(), upsert = true, multi = false)
    	
    	val key = SecurityKey.getRandKey
    	val publicKey = SecurityKey.encode(id, key)
    	
    	insertPassIntoDB(id, key, cp_Id.getOrElse(""), access_Lvl)
    	
    	Redirect(routes.UserController.userInfo()).
        withSession((USER_ID, user_id),(OTYPE, o_type),(PUBLIC_KEY, publicKey),(CP_ID, cp_Id.getOrElse("")),(ACCESS_LVL, String.valueOf(access_Lvl)))
    }else
	    Unauthorized("You are not allowed to use this service.")
  }
  
  
}