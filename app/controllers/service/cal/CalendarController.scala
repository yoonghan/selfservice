package controllers.service.cal

import play.api._
import play.api.mvc._
import controllers.service.BaseApiController
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import models.beans.CalendarModel._
import play.api.Play.current
import play.api.libs.functional.syntax._
import scala.concurrent.Future
import reactivemongo.api._
import controllers.jobs.LogActor
import controllers.service.CommonKeys._
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.annotations.ApiOperation
import com.wordnik.swagger.annotations.ApiResponses
import com.wordnik.swagger.annotations.ApiResponse
import scala.concurrent.duration._
import play.api.data.Forms._
import reactivemongo.core.commands.GetLastError
import scala.util.Failure
import scala.util.Success
import controllers.jobs.CalendarCreator
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import controllers.service.ConfigurationSetup
import models.beans.SubscriptionModel
import models.beans.UserModel.{UserProfileWithId,UserMasked,UserStorageModel,maskId,unmaskId}
import models.beans.EnumTableList
import controllers.jobs.ReportCreator
import models.beans.UserModel

/**
 * May move it as websocket in future. But concurrency now will be an issue.
 */

@Api(value = "/calendar", description = "All calendar information")
object CalendarController extends BaseApiController with MongoController{
  
  def calCollection: JSONCollection = db.collection[JSONCollection](EnumTableList.CALENDAR.toString())
  def resCollection: JSONCollection = db.collection[JSONCollection](EnumTableList.CALENDAR_SETUP.toString())
  def tmpCollection: JSONCollection = db.collection[JSONCollection](EnumTableList.CALENDAR_TEMP.toString())
  def subCollection: JSONCollection = db.collection[JSONCollection](EnumTableList.SUBSCRIPTION.toString())
  def profileCollection: JSONCollection = db.collection[JSONCollection](EnumTableList.PROFILE.toString())
  /**
   * Created to get calendar information
   */
  @ApiOperation(
    nickname = "getCalendarInformation", 
    value = "calendar", 
    notes = "Returns schedules available to user and had not yet been reserved.", 
    response = classOf[Calendar],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
	def schedules = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
	    val userId = request.session(USER_ID)
		val oType = request.session(OTYPE)
		val userName = userIDCombination(oType,userId)
		val currDate = (new DateTime(DateTimeZone.UTC)).plusHours(ConfigurationSetup.MIN_BOOKING_HR);
		
	    //retrieve only subscribed calendar
	    val querySub = Json.obj(
	    		"subs" -> Json.obj("$in" -> Json.arr(userName))
	        )
	    
	    import models.beans.SubscriptionModel._
	    
	    val fndSubCursor:Cursor[SubUserIdMapCpId] = subCollection.find(querySub).cursor[SubUserIdMapCpId]
	    val curFutureSubList: Future[List[SubUserIdMapCpId]] = fndSubCursor.collect[List]()
	    
	    curFutureSubList.flatMap{ subList =>
	      subList.size match{
	        case 0 => val empty:List[Calendar] = Nil;Future.successful(JsonResponse(Ok(Json.toJson(empty))))
	        case _ =>{
	        	val cpIds = for(subCp <- subList) yield subCp.id 
	          
			    val query = Json.obj(
				    "reg.id" -> Json.obj("$ne" -> userName), 
				    "pend.id" -> Json.obj("$ne" -> userName),
				    "avail" -> Json.obj("$gt" -> 0),
				    "start" -> Json.obj("$gt" -> currDate.getMillis()),
				    "cpId" -> Json.obj("$in" -> cpIds)
				    )
				
				val cursor:Cursor[Calendar] = calCollection.find(query).cursor[Calendar]
				val futureCalList: Future[List[Calendar]] = cursor.collect[List]()
				
			    futureCalList.map { calList =>
			    	JsonResponse(Ok(Json.toJson(calList)))
			    }
	        }
	      }
	    }
  }
  
  /**
   * Get reserved information
   */
  @ApiOperation(
    nickname = "getReservationInformation", 
    value = "reservation", 
    notes = "Returns the schedules user had already reserved", 
    response = classOf[CalendarReserved],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
	def reservedList = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
	    val userId = request.session(USER_ID)
		val oType = request.session(OTYPE)
	
		val cursor:Cursor[CalendarReserved] = calCollection.find(Json.obj("reg.id" -> userIDCombination(oType,userId))).cursor[CalendarReserved]
		val futureCalList: Future[List[CalendarReserved]] = cursor.collect[List]()
		
	    futureCalList.map { calList =>
	    	JsonResponse(Ok(Json.toJson(calList)))
	    }
  }
  
  /**
   * Get reserved information
   */
  @ApiOperation(
    nickname = "getPendingReservedEntry", 
    value = "pendingReservation", 
    notes = "Returns the schedules user wish to be reserved but pending approval", 
    response = classOf[CalendarReserved],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
	def pendingReserveList = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
	    val userId = request.session(USER_ID)
		val oType = request.session(OTYPE)
	
		val cursor:Cursor[CalendarReserved] = calCollection.find(Json.obj("pend.id" -> userIDCombination(oType,userId))).cursor[CalendarReserved]
		val futureCalList: Future[List[CalendarReserved]] = cursor.collect[List]()
		
	    futureCalList.map { calList =>
	    	JsonResponse(Ok(Json.toJson(calList)))
	    }
  }
  
  /**
   * Confirm a booking
   */
  @ApiOperation(
    nickname = "Booking", 
    value = "booking", 
    notes = "Make a Booking", 
    response = classOf[Calendar],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def reserve = AuthorizeAsyncUser(BodyParsers.parse.json){request =>    
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
	val calReservation = request.body.validate[CalReservation]
    val currDate = (new DateTime(DateTimeZone.UTC)).plusHours(ConfigurationSetup.MIN_BOOKING_HR);
    
    calReservation.fold(
        errors => {
          Logger.info(errors.toString)
          Future.successful(JsonResponse(BadRequest("Unexpected Request, what have you sent?")));
        },
        reservation => {
          
          val errorList = if(reservation.userInfo == 0) List.empty else validateSetupInput(reservation)

          if(errorList.isEmpty){
            val query = Json.obj(
                "_id"->Json.obj("$oid" -> reservation.id),			//The id 
                "avail" -> Json.obj("$gt" -> 0),					//Have availability
                "userInfo" -> reservation.userInfo,					//User Info request type
                "conf" -> reservation.conf,							//Confirm or not
                "start" -> Json.obj("$gt" -> currDate.getMillis())	//Must be after today - server time!
                )
                
                
            val storageObj = UserStorageModel(
                userIDCombination(oType,userId) ,
                reservation.addr ,
                reservation.pstCd ,
                reservation.state ,
                reservation.email ,
                reservation.ctcNo 
                )
            
            val updateRec = 
              if(reservation.conf){
                val con_jsonObj = Json.obj( "$inc" -> Json.obj( "avail" -> -1), 
                "$addToSet" -> Json.obj("pend" -> storageObj))
            	calCollection.update(query, con_jsonObj, GetLastError(), upsert = false, multi = false)
              }else{
                val con_jsonObj = Json.obj( "$inc" -> Json.obj( "avail" -> -1), 
                "$addToSet" -> Json.obj("reg" -> storageObj))
                 calCollection.update(query, con_jsonObj, GetLastError(), upsert = false, multi = false)
              }
            updateRec.map{
                result => 
                if(result.updated == 1)
                	JsonResponse(Created(Json.obj("success"->"OK")))
                else{
                  //we don't impose reason javascript should have handled it.
                  JsonResponse(Created(Json.obj("error"->"Record Not Found")))
                }
            }
          }else{
            Future.successful(JsonResponse(BadRequest(toReservationError(errorList))));
          }
        }
	   )
  }
  
   /**
   * Cancel a booking
   */
  @ApiOperation(
    nickname = "Cancellation", 
    value = "cancellation", 
    notes = "Cancel a Booking", 
    response = classOf[Calendar],
    httpMethod = "DELETE"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def unreserve = AuthorizeAsyncUser(BodyParsers.parse.json){request =>    
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
	val calCancellation = request.body.validate[CalCancellation];
		
    calCancellation.fold(
        errors => {
          Logger.info(errors.toString)
        	Future.successful(JsonResponse(BadRequest("Unexpected Request, what have you sent?")));
        },
        reservation => {
            //TODO: To calculate maximum, hackable in this way.
            removeUser(reservation.id, userIDCombination(oType,userId), Option.empty) 
        }
	   )
  }
  
  /**
   * Create a reservation.
   */
  @ApiOperation(
    nickname = "createAReservation", 
    value = "Setup a temp reservation", 
    notes = "Create a Temp Reservation - Step 1", 
    response = classOf[ReservationSetup],
    httpMethod = "PUT"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def create = AuthorizeAsyncUser(BodyParsers.parse.json, AUTH_CAL_CREATE){request =>    
    val _userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val userId = userIDCombination(oType, _userId)
    
	val reservationSetup = request.body.validate[ReservationSetup];
	
    reservationSetup.fold(
        errors => {
          Logger.error("error:-"+errors.toString)
          Future.successful(JsonResponse(BadRequest("Unexpected Request, what have you sent?")));
        },
        setup => {
          val errorList = validateSetupInput(setup)
          if(errorList.isEmpty){
            val writeObj = reservationSetupRW.writes(setup);

            val insRec = resCollection.insert((Json.obj( "userId" -> userId))  ++ writeObj)
            insRec.map{
                result =>
                if(result.ok){
                    CalendarCreator.createCalendar(setup, userId);                
                	JsonResponse(Created(Json.obj("success"->"OK")))
                }else
                	JsonResponse(Created(Json.obj("error"->"FAIL")))
            }
          }else{
            Logger.error("List of errors"+errorList);
            Future.successful(JsonResponse(BadRequest(toReservationError(errorList))));
          }
        }
	  )
  }
  
  /**
   * Retrieve reservation
   */
  @ApiOperation(
    nickname = "getTempReservation", 
    value = "List of reservation", 
    notes = "Double check all the reservation is correct, before moving. - Step 2", 
    response = classOf[ReservationSetup],
    responseContainer = "List",
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def calendarconf(from:Long, to:Long) = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
	    val userId = request.session(USER_ID)
		val oType = request.session(OTYPE)
		val userName = userIDCombination(oType,userId)
		val query = 
		  if(from == 0 && to == 0)
			  Json.obj("userId" -> userName)
		  else
			  Json.obj("userId" -> userName, "end" -> Json.obj("$gte" -> from, "$lte" -> to)) //logical to use end as base
		
		
		val cursor:Cursor[TempCalendar] = tmpCollection.find(query).cursor[TempCalendar]
		val futureCalList: Future[List[TempCalendar]] = cursor.collect[List]()
		
	    futureCalList.map { calList =>
	    	JsonResponse(Ok(Json.toJson(calList)))
	    }
  }
  
  /**
   * Create reservation
   */
  @ApiOperation(
    nickname = "publishReservation", 
    value = "Publish temp reservation as reservation", 
    notes = "Publishes the reservation via backend. - Step 3.0", 
    response = classOf[ReservationSetup],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def copySetup = AuthorizeUser(BodyParsers.parse.json, AUTH_CAL_CREATE){request =>    
	    val userId = request.session(USER_ID)
	    val cpId = request.session(CP_ID)
		val oType = request.session(OTYPE)
		val userName = userIDCombination(oType,userId)
		val tempCal = request.body.validate[List[TempCalendar]];
	    
	    tempCal.fold(
	        errors => {
	          Logger.error("CopySetup Errors:-"+errors.toString)
	          JsonResponse(BadRequest("Unexpected Request, what have you sent?"))
	        },
	        setup => {
	          CalendarCreator.makeLiveCalendar(setup, cpId, userName)
	          JsonResponse(Created(Json.obj("success"->"OK")))
	        }
  		)
  }
  
  /**
   * Cancel reservation creation
   */
  @ApiOperation(
    nickname = "removeTempReservation", 
    value = "Delete the temp reservations", 
    notes = "Removes the reservation automatically. - Step 3.1", 
    response = classOf[ReservationSetup],
    httpMethod = "DELETE"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def deleteSetup = AuthorizeUser(BodyParsers.parse.json){request =>    
	    val userId = request.session(USER_ID)
		val oType = request.session(OTYPE)
		val userName = userIDCombination(oType,userId)
	    
      CalendarCreator.removeLiveCalendar(userName)
      JsonResponse(Created(Json.obj("success"->"OK")))
  }
  
  /**
   * Request for the current generated status
   */
  @ApiOperation(
    nickname = "getReservationProgress", 
    value = "reservation", 
    notes = "Check reservation processing status", 
    response = classOf[ReservationSetup],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def reservationProgress = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
	    val userId = request.session(USER_ID)
		val oType = request.session(OTYPE)
		val userName = userIDCombination(oType,userId)
	    val query = Json.obj("userId" -> userName)
	    val sort = Json.obj("count" -> -1)
		
		val cursor:Cursor[TempCalendar] = tmpCollection.find(query).sort(sort).cursor[TempCalendar]
		val futureCalList: Future[List[TempCalendar]] = cursor.collect[List](1,false)
		
	    val response = futureCalList.map { calList =>
	    	JsonResponse(Ok(Json.toJson(calList)))
	    }
	    response
  }
  
  /**
   * Shows the list of reservations details
   */
  @ApiOperation(
    nickname = "reservationlist", 
    value = "reservation", 
    notes = "Check reservation processing status", 
    response = classOf[CalendarRegisteredUser],
    responseContainer = "List",
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def reservationList = AuthorizeAsyncUser(BodyParsers.parse.anyContent,AUTH_CAL_CREATE){ request =>
	import models.beans.SubscriptionModel._
    val cpId = request.session(CP_ID)
	
	
    val query = Json.obj(
        "cpId" -> cpId, 
        "$or" -> Json.arr(
            Json.obj("reg"-> Json.obj("$exists" -> true, "$not" -> Json.obj("$size" -> 0))),
            Json.obj("pend"-> Json.obj("$exists" -> true, "$not" -> Json.obj("$size" -> 0)))
            )
        )
    val cursor:Cursor[Calendar] = calCollection.find(query).cursor[Calendar]
    val futureCalList: Future[List[Calendar]] = cursor.collect[List]()
    
    val resp = futureCalList.map { calList =>
      JsonResponse(Ok(Json.toJson(calList)))
    }
    resp

  }
  
  /**
   * Show user details per reservation detail
   */
  @ApiOperation(
    nickname = "getUsersInReservation", 
    value = "Get user detail per reservation", 
    notes = "Get user details per reservation, call getReservationDetails", 
    response = classOf[UserMasked],
    responseContainer = "List",
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def usersInReservations(id:String) = AuthorizeAsyncUser(BodyParsers.parse.anyContent, AUTH_CAL_CREATE){ request =>   
    
	    val userId = request.session(USER_ID)
		val oType = request.session(OTYPE)
		val userName = userIDCombination(oType,userId)
		val cpId = request.session(CP_ID)
//		val tempCal = request.body.validate[CalReservation];
//	
//		tempCal.fold(
//	        errors => {
//	          Logger.error("error:-"+errors.toString)
//	          Future.successful(JsonResponse(BadRequest("Unexpected Request, what have you sent?")))
//	        },
//			cal => {
		      val query = Json.obj("_id" -> Json.obj("$oid"-> id ), "cpId" -> cpId)
		      
		      val cursor:Cursor[CalendarRegisteredUser] = calCollection.find(query).cursor[CalendarRegisteredUser]
		      val futureCal: Future[List[CalendarRegisteredUser]] = cursor.collect[List](1)
		      
		      futureCal.flatMap { cal => 
				cal.size match {
				  case 1 => {
				    //Need to have both reg and pend, this is wrong!
				    val registeredUsers:List[UserStorageModel] = {
				      if(cal(0).reg.isDefined)
				        cal(0).reg.get
				      else
				        Nil
				    }
				    val pendingUsers:List[UserStorageModel] = {
				      if(cal(0).pend.isDefined)
				        cal(0).pend.get
				      else
				         Nil
				    }
				    val queryIns = Json.obj(
				        "_id" -> Json.obj("$in" -> 
				        (registeredUsers.map( storageInfo => storageInfo.id) ++ pendingUsers.map( storageInfo => storageInfo.id))
				        ))
				        
				    val cursor:Cursor[UserProfileWithId] = profileCollection.find(queryIns).cursor[UserProfileWithId]
				    val futureCalList: Future[List[UserProfileWithId]] = cursor.collect[List]()
				    
				    //Store all into maps
				    val pendingMap = pendingUsers.map(t => t.id -> t).toMap
				    val registeredMap = registeredUsers.map(t => t.id -> t).toMap
				    
				    futureCalList.map { calList =>
				      val masked = calList.map( cal =>{
				        
				        val user = registeredMap.get(cal._id).getOrElse(pendingMap.get(cal._id).get) //will always exist in either

				        UserMasked(
				            pendingMap.contains(cal._id),
				            cal.firstName,
				            cal.lastName,
				            user.addr,
				            user.pstCd,
				            user.state,
				            user.email ,
				            user.ctcNo,
				            maskId(cal._id))
				      }
				      )
				      JsonResponse(Ok(Json.toJson(masked)))
				    }
				  }
				  case _ => {
				    Future.successful(JsonResponse(BadRequest(Json.obj("error"->"No such record"))))
				  }
				}
		      }
//			}
//		)
  }
  
  
  
  
  /**
   * Cancel a booking by CP
   */
  @ApiOperation(
    nickname = "Cancellation", 
    value = "Cancellation by CP", 
    notes = "Cancel a Booking by CP", 
    response = classOf[Calendar],
    httpMethod = "DELETE"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def cmd_unreserve = AuthorizeAsyncUser(BodyParsers.parse.json, AUTH_CAL_CREATE){request =>    
	val calReservation = request.body.validate[CalCmdReserve];
	val cpId = request.session(CP_ID)
    	
    calReservation.fold(
        errors => {
          Logger.info(errors.toString)
        	Future.successful(JsonResponse(BadRequest("Unexpected Request, what have you sent?")));
        },
        reservation => {
			val userId = unmaskId(reservation.userId )
			removeUser(reservation.id, userId, Option(cpId)) 
        }
	   )
  }
  
   /**
   * Confirm pending user by CP
   */
  @ApiOperation(
    nickname = "ConfirmUser", 
    value = "Confirm by CP", 
    notes = " Confirm pending user booking", 
    response = classOf[Calendar],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def cmd_reserve = AuthorizeAsyncUser(BodyParsers.parse.json, AUTH_CAL_CREATE){request =>
    val currDate = (new DateTime(DateTimeZone.UTC)).plusHours(ConfigurationSetup.MIN_BOOKING_HR);
	val calReservation = request.body.validate[CalCmdReserve];
	val cpId = request.session(CP_ID)
    	
    calReservation.fold(
        errors => {
          Logger.info(errors.toString)
        	Future.successful(JsonResponse(BadRequest("Unexpected Request, what have you sent?")));
        },
        reservation => {
			val userId = unmaskId(reservation.userId )
			val query = Json.obj("conf" -> true,
						"_id" -> Json.obj("$oid" -> reservation.id),
						"start" -> Json.obj("$gt" -> currDate.getMillis()),
						"pend.id" -> userId)

			val cursor:Cursor[CalendarRegisteredUser] = calCollection.find(query).cursor[CalendarRegisteredUser]
			val futureCalList: Future[List[CalendarRegisteredUser]] = cursor.collect[List]()
			futureCalList.flatMap{
			  q_result =>{

			    if(q_result.size==1){
			    	val result = q_result(0).pend.get.find(pendingUser => pendingUser.id == userId)
			    	val con_jsonObj = Json.obj( "$addToSet" -> Json.obj("reg" -> result.get),
				      "$pull" -> Json.obj("pend" -> Json.obj("id" -> userId))
				      )
				    val updateRec = calCollection.update(query, con_jsonObj, GetLastError(), upsert = false, multi = false) 
			        val w_response = updateRec.map{
			    	  w_result => {
			    	    if(w_result.updated == 1){
			    	    	JsonResponse(Created(Json.obj("success"->"OK")))
			    	    }else{
			    	      Logger.info(Json.stringify(query))
			    	    	JsonResponse(BadRequest(Json.obj("error"->"We do not process past dates or there is no event found.")))
			    	    }
			    	  }
			    	}
			    	w_response
			    }else{
			      Future.successful(JsonResponse(BadRequest(Json.obj("error"->"We do not process past dates or there is no event found."))))
			    }
			  }
			}
       })
  }
  
  
  private def removeUser(reservationId:String, userId:String, cpId:Option[String]):Future[play.api.mvc.Result]={
    
    val currDate = (new DateTime(DateTimeZone.UTC)).plusHours(ConfigurationSetup.MIN_BOOKING_HR);
    
    val query =
      if(cpId.isEmpty){
	      Json.obj(
		      "_id" -> Json.obj("$oid" -> reservationId),
		      "start" -> Json.obj("$gt" -> currDate.getMillis()),
		      "$or" -> Json.arr(Json.obj("reg.id" -> userId), Json.obj("pend.id" -> userId))
		      )
      }else{
        Json.obj(
	      "_id" -> Json.obj("$oid" -> reservationId),
	      "start" -> Json.obj("$gt" -> currDate.getMillis()),
	      "$or" -> Json.arr(Json.obj("reg.id" -> userId), Json.obj("pend.id" -> userId)),
	      "cpId" -> cpId.get
	      )
      }
    
	val con_jsonObj = Json.obj( "$inc" -> Json.obj( "avail" -> 1),
	      "$pull" -> Json.obj("reg" -> Json.obj("id" -> userId)),
	      "$pull" -> Json.obj("pend" -> Json.obj("id" -> userId))
	      )
	val updateRec = calCollection.update(query, con_jsonObj, GetLastError(), upsert = false, multi = false)
	val response = updateRec.map{
	result => {
		if(result.updated == 1)
			JsonResponse(Created(Json.obj("success"->"OK")))
		else
	        JsonResponse(Created(Json.obj("error"->"We do not process past dates or there is no event found.")))
		}
	}
	response
  } 
}