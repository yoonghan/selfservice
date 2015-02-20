package controllers.service.rest

import play.api._
import play.api.mvc._
import controllers.service.BaseApiController
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import models.beans.ReminderModel._
import models.beans.EmailNotifyModel._
import play.api.Play.current
import play.api.libs.functional.syntax._
import scala.concurrent.Future
import reactivemongo.api._
import controllers.service.CommonKeys._
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.annotations.ApiOperation
import com.wordnik.swagger.annotations.ApiResponses
import com.wordnik.swagger.annotations.ApiResponse
import scala.concurrent.duration._
import play.api.data.Forms._
import models.beans.EnumTableList.{REMINDER,EMAIL_VALID_LIST,EMAIL_NOTIFY_LIST }
import models.auth.SecurityKey
import java.util.UUID
import reactivemongo.core.commands.GetLastError
import org.joda.time.DateTimeZone
import org.joda.time.DateTime

@Api(value = "/reminder", description = "User Reminder Information")
object ReminderController extends BaseApiController with MongoController {
	def reminderCollection: JSONCollection = db.collection[JSONCollection](REMINDER.toString())
	def emailValidCollection: JSONCollection = db.collection[JSONCollection](EMAIL_VALID_LIST.toString())
	def emailCollection: JSONCollection = db.collection[JSONCollection](EMAIL_NOTIFY_LIST.toString())
	
	val dateFormat = "dd/MM/yyyy";
	
  /**
   * Insert or update reminder profile
   */
  @ApiOperation(
    nickname = "insertReminderProfile", 
    value = "Creates or updates a user reminder", 
    notes = "Creates or update a user reminder", 
    response = classOf[String],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def profileIns = AuthorizeUser(BodyParsers.parse.json){request =>
    def insertToEmailValidCollection(userId:String, email:Option[String]){
      if(email.isDefined){
          val resetDateWithoutHours = getDate
          val insDate = resetDateWithoutHours.toString(dateFormat)
        
    	  val userAuthId = Json.obj("_id" -> userId)
    	  val key = UUID.randomUUID().toString();
		  val emailObj = EmailValidate(
		      userId,
		      email.get,
		      key,
		      resetDateWithoutHours.getMillis(),
		      false
		      )
		  val update = emailValidCollection.update(userAuthId, Json.obj("$set"->emailObj), GetLastError(), upsert = true, multi = false)
		  createEmail(userId, email.get, key:String, insDate)
      }
    }
    
    val _reminderProfile = request.body.validate[ReminderSetting_Edit];
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    
    _reminderProfile.fold(
        errors => {
          JsonResponse(BadRequest(Json.obj("error"->"Unexpected Request, what have you sent?")));
        },
        reminderProfile => {
          val errorList = validateInput(reminderProfile)
          if(errorList.isEmpty){
            val userCombination = userIDCombination(oType , userId)
            val userAuthId = Json.obj("_id" -> userCombination)
            
            val query = if(reminderProfile.alertEmail.isDefined){
              (userAuthId ++ Json.obj("alertEmail" -> reminderProfile.alertEmail.get)) 
            }else{
              userAuthId
            }
        
            val jsonObj = Json.toJson(reminderProfile)
            val upd_jsonObj = jsonObj.as[JsObject] ++ userAuthId
            
            val update = reminderCollection.update(query, upd_jsonObj, GetLastError(), upsert = false, multi = false)
            
            val message = update.map{
	          result => {
	        	  if(result.updated == 0){
	        	    insertToEmailValidCollection(userCombination, reminderProfile.alertEmail)
	        	    val mod_UpdObj = upd_jsonObj ++ Json.obj("validEmail"->false)
	        	    reminderCollection.update(userAuthId, mod_UpdObj, GetLastError(), upsert = true, multi = false)
	          	  }
	          }
	        }
        	JsonResponse(Created(Json.obj("success"->"OK")))
          }else{
            JsonResponse(BadRequest(toReminderSettingError(errorList)));
          }
        }
     )
  }
	
  /**
   * Resend email reminder. 
   */
  @ApiOperation(
    nickname = "resendemail", 
    value = "Resent email reminder", 
    notes = "If user have an email alert, resent the email reminder again.", 
    response = classOf[ReminderSetting],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def resendEmailReminder = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val userCombination = userIDCombination(oType , userId)
    val query = Json.obj("_id" -> userCombination)
    
    
	val cursor:Cursor[EmailValidate] = emailValidCollection.find(query).cursor[EmailValidate]
	val futureEmailValidate: Future[List[EmailValidate]] = cursor.collect[List]()
	
    futureEmailValidate.map { reminderList =>
      reminderList.size match {
        case 1 => {
          val insDate = getDate.toString(dateFormat)
          createEmail(userCombination, reminderList(0).email , reminderList(0).key , insDate)
          JsonResponse(Ok(Json.obj("success" -> "ok")))
        }
        case _ => {
          JsonResponse(Ok(Json.obj("error" -> "Record not found")))
        }
      }
    }
  }
  
  /**
   * Validate email reminder
   */
  @ApiOperation(
    nickname = "validateemail", 
    value = "Validate Email Sent", 
    notes = "validate the user's email that have been sent.", 
    response = classOf[ReminderSetting],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def validateEmailReminder(userId:String, key:String) = Action.async(BodyParsers.parse.anyContent){request =>
    val query = Json.obj("_id" -> userId, "key" -> key)
    
    val futureRemove = emailValidCollection.remove(query)
    futureRemove.map{
      result => {
        //use redirect
    	if(result.updated == 1){
    		val upd_query = Json.obj("_id" -> userId)
    		val updateObj = Json.obj("$set"->Json.obj("validEmail"->true))
    	    val futureUpdate = reminderCollection.update(upd_query, updateObj, GetLastError(), upsert = false, multi = false)
    		JsonResponse(Ok(Json.obj("success" -> "ok")))
      	}else
      	   JsonResponse(Ok(Json.obj("error" -> "Not ok")))
      }
    }
  }
	
	
  /**
   * Retrieve reminder profile by user
   */
  @ApiOperation(
    nickname = "getReminderProfile", 
    value = "Reminder Profile", 
    notes = "Returns user's reminder profile", 
    response = classOf[ReminderSetting],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def profile = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
    
	val userId = request.session(USER_ID)
	val oType = request.session(OTYPE)
	val query = Json.obj("_id" -> userIDCombination(oType,userId));

	val cursor:Cursor[ReminderSetting] = reminderCollection.find(query).cursor[ReminderSetting]
	val futureReminderList: Future[List[ReminderSetting]] = cursor.collect[List]()
	
    futureReminderList.map { reminderList =>
      reminderList.size match {
        case 0 => JsonResponse(Ok(Json.obj("success" -> "ok")))
        case _ => {
          JsonResponse(Ok(Json.toJson(reminderList(0))))
        }
      }
    }
  }
  
  private def getDate():DateTime={
    val currDate = (new DateTime(DateTimeZone.UTC))
    new DateTime(currDate.getYear(), currDate.getMonthOfYear(), currDate.getDayOfMonth(), 0, 0, 0)
  }
  
  /**
   * If there are duplicates, any changes will get overridden until email gets sent. 
   */
  private def createEmail(userId:String, userEmail:String, key:String, date:String){
    //no decoder needed until in future.
    val link = s"u=$userId&k=$key"
    val id = "EMAIL"+userId
    val query = Json.obj("_id"-> id)
    
    val emailMessage = "Hello there,\n\n" +
    		"You are receiving this email as your email in JOM Jaring has not been verified\n"+
    		"\n\n"+
    		s"Please verify your account by click on this link <a href='http://www.jomjaring.com/selfservice/verify?$link'>here</a>\n\n"+
    		s"or manually trigger the link with http://www.jomjaring.com/reminder/verify?$link\n\n"+
    		"Reminder: Beware of fraudelant emails. We from JOM Jaring do not imply any charges from you for this service is provided free.\n\n"+
    		"Sincerity from,\n"+
    		"JOM Jaring";
    
    val notifyObj = EmailNotify(
        id,
        userId, 
        date, 
        userEmail, 
        emailMessage, 
        false
        )
    val updRec = emailCollection.update(query,notifyObj, GetLastError(), upsert=true, multi=false)
    updRec.onComplete{
      case scala.util.Failure(f) => Logger.error("Failure in notifying user:["+userId+"]->"+f)
      case scala.util.Success(success) => ;
    }
  }
}