package controllers.service.rest

import play.api._
import play.api.mvc._
import controllers.service.BaseApiController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import models.beans.SubscriptionModel._
import models.beans.UserModel.{UserLogin,User}
import scala.concurrent.Future
import reactivemongo.api._
import utils.CommonKeys._
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.annotations.ApiOperation
import com.wordnik.swagger.annotations.ApiResponses
import com.wordnik.swagger.annotations.ApiResponse
import reactivemongo.core.commands.GetLastError
import reactivemongo.bson.{BSONObjectID}
import models.beans.EnumTableList.{SUBSCRIPTION,USER }
import controllers.jobs.LogActor
import utils.ConfigurationSetup
import java.nio.file.{Files,StandardCopyOption}
import utils.Utility

@Api(value = "/subscription", description = "Subscription Information")
object SubscriptionController extends BaseApiController {

  def subscriptionCollection: JSONCollection = db.collection[JSONCollection](SUBSCRIPTION.toString())
  def userCollection: JSONCollection = db.collection[JSONCollection](USER.toString())

  /**
	* Query all subscription list.
  */
  @ApiOperation(
    nickname = "getSubscriptionList",
    value = "SubscriptionList",
    notes = "Returns available subscriptionList",
    response = classOf[SubscriptionHost],
    responseContainer = "List",
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def hostlist = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>

    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val query = Json.obj("status" -> STATUS_ACTIVE)
    val userAuth = userIDCombination(oType, userId)

	/**HALTED - This is only working in Mongo 2.6 onwards**/
//	val command = Aggregate ("subscription", Seq(
//	    Match(BSONDocument("status" -> 1)),
//
//    	Project(
//    	    ("id",BSONInteger(1)),
//    	    ("cName",BSONInteger(1)),
//    	    ("cDesc",BSONInteger(1)),
//    	    ("subscribed",
//    	        BSONDocument("$setIsSubset"->BSONArray(
//    	            BSONArray(userAuth), "$subs")))
//    	    )
//    	 ))
//    /**
//     * --Query Execution
//     * db.subscription.aggregate([{$match:{status:1}},{$project:{"userId":1, "cId":1,"subs":1, discount:{$setIsSubset:[["G114852108498604797792"],"$subs"]}}}])
//	**/
//
//    import play.modules.reactivemongo.json.BSONFormats._
//	val result = db.command(command)
//
//    val futureSubscriptionList = result.map { value => {
//        value.toSeq map (Json.toJson(_).as[SubscriptionReg])
//      }}
//
//    futureSubscriptionList.map { subscriptionList =>
//      subscriptionList.size match {
//        case 0 => JsonResponse(NotFound(Json.obj()))
//        case _ => {
//          JsonResponse(Ok(Json.toJson(subscriptionList)))
//        }
//      }
//    }
    /**HALTED - This is only working in Mongo 2.6 onwards**/
	/**REPLACEMENT until Mongo 2.6 works[S]**/
    val cursor: Cursor[SubscriptionHost] = subscriptionCollection.find(query).cursor[SubscriptionHost]

    val futureSubscriptionReg: Future[List[SubscriptionHost]] = cursor.collect[List]()

    futureSubscriptionReg.map { subscriptionList =>
      subscriptionList.size match {
        case 0 => JsonResponse(NotFound(Json.obj()))
        case _ => {
          val returnedList = subscriptionList.map( subList => {

            val isSubscribed =  subList.subs.getOrElse(Nil).contains(userAuth)

            SubscriptionReg(
                subList.id ,
                subList.cName ,
                subList.cDesc ,
                subList.ext ,
                isSubscribed
                )
          })
          JsonResponse(Ok(Json.toJson(returnedList)))
        }
      }
      /**REPLACEMENT until Mongo 2.6 works[E]**/
    }
  }

	/**
	 * Update subscription list
	 */
  @ApiOperation(
    nickname = "updSubscriptionList",
    value = "JSONResponse",
    notes = "Update user subscribed on list subscribed",
    response = classOf[String],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def updateHostList = AuthorizeAsyncUser(BodyParsers.parse.json){request =>

    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val query = Json.obj("del" -> false)
    val userAuth = userIDCombination(oType, userId)

    val subscribedIds = request.body.validate[SubscribedIds]

    subscribedIds.fold(
      errors => {
                Logger.info(errors.toString)
                Future.successful(JsonResponse(BadRequest("Unexpected Request, what have you sent?")))
              },
      subId => {
	/**HALTED - This is only working in Mongo 2.6[S] onwards**/
//		  val remapSub = subId.subs.map( x  => BSONObjectID(x))
//		  val remapUnSub = subId.unsubs.map( x  => BSONObjectID(x))
//
//		  val ins_query = BSONDocument("_id" -> BSONDocument("$in"-> (remapSub)))
//		  val del_query = BSONDocument("_id" -> BSONDocument("$in"-> (remapUnSub)))
//
//		  val ins_conJsonObj = BSONDocument("$addToSet" -> BSONDocument("subs" -> userIDCombination(oType,userId)))
//		  val del_conJsonObj = BSONDocument("$pull" -> BSONDocument("subs" -> userIDCombination(oType,userId)))
//
//		  val commandDoc =
//	        BSONDocument(
//	          "update" -> subscription_CollectionName,
//	          "updates" -> BSONArray(
//	            BSONDocument("q" -> ins_query, "u" -> ins_conJsonObj, "upsert"->false, "multi"->true),
//	            BSONDocument("q" -> del_query, "u" -> del_conJsonObj, "upsert"->false, "multi"->true)
//	          )
//	        )
//
//	        /**
//	         * Example query
//	         * db.runCommand({update:"subscription", updates: [
//					{q:{cId:{$in:[]}}, u: {$addToSet: {subs: "G114852108498604797792"}}},
//					{q:{cId:{$in:["Id1", "Id2", "Id1", "Id1", "Id1", "Id11"]}}, u: {$pull: {subs: "G114852108498604797792"}},upsert:false, multi:true}
//				]})
//	         */
//
//	        val updateRec = db.command(RawCommand(commandDoc))
//	        updateRec.onFailure{
//	            case f => {
//	            	//manually handle exception bug https://groups.google.com/forum/#!topic/reactivemongo/xKWO8aXbYnE
//	            	if(f.getMessage().contains("writeError")){
//	            	  f.printStackTrace()
//	            	}
//	            }
//			  }
//
//
//
	/**HALTED - This is only working in Mongo 2.6[E] onwards**/
	/**REPLACEMENT until Mongo 2.6 works[S]**/

		  val remapSub = subId.subs.map( x  => Json.obj("$oid"-> x))
		  val remapUnSub = subId.unsubs.map( x  => Json.obj("$oid"-> x))

		  val insQuery = Json.obj("_id" -> Json.obj("$in" -> remapSub ))
		  val delQuery = Json.obj("_id" -> Json.obj("$in" -> remapUnSub ))
		  val ins_conJsonObj = Json.obj("$addToSet" -> Json.obj("subs" -> userIDCombination(oType,userId)))
		  val del_conJsonObj = Json.obj("$pull" -> Json.obj("subs" -> userIDCombination(oType,userId)))
		  val futureIns = subscriptionCollection.update(insQuery, ins_conJsonObj, GetLastError(), upsert = false, multi = true)
		  val futureDel = subscriptionCollection.update(delQuery, del_conJsonObj, GetLastError(), upsert = false, multi = true)

		  futureIns.onFailure{
		    case f => {
		      LogActor.logActor ! "Error insertion on subscription for: "+userId+" >> "+f.getMessage()
		      f.printStackTrace()
		    }
		  }
		  futureDel.onFailure{
		    case f => {
		      LogActor.logActor ! "Error deleting subscription for: "+userId+" >> "+f.getMessage()
		      f.printStackTrace()
		    }
		  }
	/**REPLACEMENT until Mongo 2.6 works[E]**/
		  Future.successful(JsonResponse(Created(Json.obj("success"->"ok"))))
  	})
  }

  def createSubscription(userId:String, oType:String, authLvl:Int, sub:Subscription):Future[Boolean]={

    val userAuth = userIDCombination(oType, userId)

    val query = Json.obj("$or"->Json.arr(
        Json.obj("userId" -> userAuth),Json.obj("cName" -> sub.cName.trim())
        ))
    val cursor: Cursor[SubUserIdMapCpId] = subscriptionCollection.find(query).cursor[SubUserIdMapCpId]

    val futureQuerySubscription: Future[List[SubUserIdMapCpId]] = cursor.collect[List](1)
    futureQuerySubscription.flatMap{ queryRec =>
      if(queryRec.size==0){

        val bsonId = BSONObjectID.generate.stringify
        val createSub = Subscription_Creation(bsonId, sub.cName , sub.cDesc , sub.cWebsite ,sub.cCtcNo,sub.cEmail , List(userAuth), userAuth, 2, 0)
        val futureInsSubscription = subscriptionCollection.insert(createSub)
        futureInsSubscription.flatMap(
            status => {
              if(status.ok){
                val query = Json.obj("id" -> userId, "otype" -> oType)
                val updateVal = Json.obj("$set"->Json.obj("cpId" -> bsonId,"authLevel" -> ( AUTH_CAL_CREATE_LVL | authLvl )))
                val futureUpdateUser = userCollection.update(query, updateVal, GetLastError(), false, false)
                futureUpdateUser.map{
                  status =>
                  if(status.updated == 1){
                    val query = Json.obj("_id" -> Json.obj("$oid" -> bsonId))
                    val updateSub = Json.obj("$set"->Json.obj("status" -> 1))
                    subscriptionCollection.update(query, updateSub, GetLastError(), false, false)
                    true
                  }else{
                    false
                  }
                }
              }else
                Future.successful(false)
            }
        )
      }else{
        Future.successful(false)
      }
    }
  }

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
  def profile = AuthorizeAsyncUser(BodyParsers.parse.empty, AUTH_CAL_CREATE_LVL){request =>

    val cpId = request.session(CP_ID)

    val query = Json.obj("_id" -> Json.obj("$oid"->cpId))
	val cursor:Cursor[Subscription] = subscriptionCollection.find(query).cursor[Subscription]
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

    val cpProfile = request.body.validate[Subscription_Edit]
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val cpId = request.session(CP_ID)

    cpProfile.fold(
        errors => {
          Future.successful(JsonResponse(BadRequest(Json.obj("error"->"Unexpected Request, what have you sent?"))))
        },
        cpProfile => {
          val errorList = validateSubscription_Edit(cpProfile)
          if(errorList.isEmpty){
            val jsonObj = Json.toJson(cpProfile)
            val modifiedObj = jsonObj.as[JsObject].-("ver")

            val query = Json.obj("ver" -> cpProfile.ver, "_id" -> Json.obj("$oid"->cpId))
            val updateQuery = Json.obj("$set" -> modifiedObj, "$inc" -> Json.obj( "ver" -> 1))
            val futureUpdate = subscriptionCollection.update(query, updateQuery, GetLastError(), upsert = false, multi = false)
            futureUpdate.map{ lastError =>
              if(lastError.updated == 1)
                JsonResponse(Created(Json.obj("success"->"OK")))
              else
                JsonResponse(NotFound(Json.obj("errors"->Json.arr("Someone has done a concurrent modification on this profile. Please reset to retrieve latest profile."))))
            }
          }else{
            Future.successful(JsonResponse(BadRequest(toSubscriptionError(errorList))))
          }
        }
      )
  }

  @ApiOperation(
    nickname = "allowUserToManage",
    value = "Allow user to manage",
    notes = "Allow user to manage.",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def subscribeManager = AuthorizeAsyncUser(BodyParsers.parse.json, AUTH_CAL_CREATE_LVL){request =>
    val cpId = request.session(CP_ID)
    val inputLogin = request.body.validate[UserLogin]

    inputLogin.fold(
        errors => {
          Future.successful(JsonResponse(BadRequest(Json.obj("error"->"Unexpected Request, what have you sent?"))))
        },
        userProfile => {
          if(userProfile.email.isDefined){

            val query = Json.obj("otype" -> userProfile.otype, "email" -> userProfile.email, "newUser" -> false )
            val cursor:Cursor[User] = subscriptionCollection.find(query).cursor[User]
            val futureProfileList: Future[List[User]] = cursor.collect[List]()

            futureProfileList.flatMap { profileList =>
              profileList.size match {
                case 1 => {
                  val authLevel = profileList(0).authLevel.getOrElse(AUTH_DEFAULT_LVL )
                  val query = Json.obj("id" -> profileList(0).id, "otype" -> userProfile.otype)
                  val updateQuery = Json.obj("$set"->Json.obj("cpId" -> cpId,"authLevel" -> ( AUTH_CAL_CREATE_LVL |  authLevel )))
                  val futureUpdate = userCollection.update(query, updateQuery, GetLastError(), upsert = false, multi = false)
                  futureUpdate.map{ lastError =>
                    if(lastError.updated == 1)
                      JsonResponse(Created(SUC_COMMON_OK))
                    else
                      JsonResponse(NotFound(JSON_KEYWORD_ERRORS("User not found")))
                  }
                }
                case _ => {
                  Future.successful(JsonResponse(NotFound(JSON_KEYWORD_ERRORS("User not found"))))
                }
              }
            }

          }else{
            Future.successful(JsonResponse(BadRequest(JSON_KEYWORD_ERRORS("Email is Empty"))))
          }
        }
      )
  }


  /**
   * Add image to subscription
   */
  @ApiOperation(
    nickname = "ImageSubscription",
    value = "Add image to subscription",
    notes = "Add image to subscription",
    response = classOf[String],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def uploadImage() = AuthorizeAsyncUser(BodyParsers.parse.json, AUTH_CAL_CREATE_LVL){request =>

    def copyImage(from_ImgName:String, to_ImgName:String):Boolean = {
      val successful = try{
	      val fromFile = new java.io.File(ConfigurationSetup.FOLDER_TEMP + ConfigurationSetup.FOLDER_PICTURE + from_ImgName)
	      val toFile = new java.io.File(ConfigurationSetup.FOLDER_STORE + ConfigurationSetup.FOLDER_PICTURE + to_ImgName)
	      Files.move(fromFile.toPath(), toFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
	      true
      }catch{
      	case e:Exception => Logger.error("imgName:" + e.getMessage()); false
      }
      successful
    }

    val msgInput = request.body.validate[SubscriptionImg]
    msgInput.fold(
        errors => {
          Future.successful(JsonResponse(BadRequest(JSON_KEYWORD_ERRORS("Unexpected Request, what have you sent?"))))
        },
        subImg => {
          val ext = subImg.ext.get
          val ver = subImg.ver
          val cpId = request.session(CP_ID)
          val userAuth = userIDCombination(request.session(OTYPE), request.session(USER_ID))

          Logger.info(ext+":"+ver+":"+cpId+":")

		  if(ext == ".jpg" || ext == ".jpeg" || ext == ".png"){
		    if(copyImage(userAuth+ext, cpId+ext)){
			    val query = Json.obj("ver"->ver, "_id"->Json.obj("$oid"->cpId))
			    val updateObj = Json.obj("$set" -> Json.obj("ext" -> ext), "$inc" -> Json.obj( "ver" -> 1))
			    val futureUpdate = subscriptionCollection.update(query, updateObj, GetLastError(), false, false)
			    futureUpdate.map{ lastError =>
			    if(lastError.updated == 1)
			    	JsonResponse(Created(SUC_COMMON_OK))
			    else
			    	JsonResponse(NotFound(JSON_KEYWORD_ERRORS("Records have been updated. Please refresh.")))
			    }
		    }else{
		      Future.successful(JsonResponse(NotFound(JSON_KEYWORD_ERRORS("File has been updated. Please refresh."))))
		    }
		  }else{
		    Future.successful(JsonResponse(BadRequest(JSON_KEYWORD_ERRORS("Unsupported file type."))))
		  }
        })
  }

  @ApiOperation(
    nickname = "CPImage",
    value = "Show CP image",
    notes = "Show CP image",
    response = classOf[String],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def getCPImage() = AuthorizeAsyncUser(BodyParsers.parse.empty){
    request => {
      val cpId = request.session(CP_ID)
      val query = Json.obj("_id"->Json.obj("$oid"->cpId), "ext"-> Json.obj("$exists" -> true))
      val cursor: Cursor[SubscriptionImg] = subscriptionCollection.find(query).cursor[SubscriptionImg]

      val futureCpImg: Future[List[SubscriptionImg]] = cursor.collect[List]()

      futureCpImg.map { cpImg =>
	      cpImg.size match {
	        case 0 => ImageResponse(".gif", Utility.getImageURL("no+image","100","100"))
	        case _ => {
	          val ext = cpImg(0).ext.get
	          ImageResponse(ext, Utility.getImage(cpId+ext))
	        }
	      }
      }
    }
  }

  def getImage(ext:String, file:String) = AuthorizeUser(BodyParsers.parse.anyContent){
    request=>
      ImageResponse(ext, Utility.getImage(file+ext), 3600)
  }

}

