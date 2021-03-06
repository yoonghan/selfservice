package controllers.service.test

import controllers.service.BaseApiController
import controllers.service.rest.SubscriptionController._
import models.auth.SecurityKey
import models.beans.EnumTableList._
import models.beans.SubscriptionModel.Subscription
import models.beans.CalendarModel.CalendarWithoutId
import models.beans.UserModel.{UserProfileWithId,User}
import play.api.Play._
import play.api.libs.json.{JsBoolean, Json}
import play.api.mvc.{Action, Result}
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.concurrent.duration._
import reactivemongo.core.commands.GetLastError
import utils.CommonKeys._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.util.{Success,Failure}
import org.joda.time.{DateTime,DateTimeZone}
import reactivemongo.api.Cursor
import scala.concurrent.Future

import play.api.mvc._

object TestAction extends ActionBuilder[Request] {
  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) = {
    val value:Option[String] = current.configuration.getString("environment")
    if(value.isDefined && value.get == "test") {
      block(request)
    }else{
      Future.successful(Forbidden("Unauthorized access"))
    }
  }
}

/**
 * Created by yoong.han on 3/31/2015.
 */
object TestHelper extends BaseApiController{
  /**
   * This method is only available for testing as oauth is only workable for production
   * 1. Updates the records.
   * 2. Send the records as a cookie.
   */
  def testprofile(code:Int) = TestAction {
    val _userid = "114852108498604797792"
    val _name = "Test Data"
    val _type = "G"
    val _accessLvl = AUTH_CAL_CREATE_LVL + AUTH_DEFAULT_LVL
    val _cpId = "5702efad36864f8100e16f64"
    val _newUser = false

    code match {
      case 1 => {
        //user with all access
        runMe(_userid, _name, _type, _accessLvl, Option(_cpId), _newUser)
      }
      case 2 => {
        //user with new access
        val _type = "F"
        val _cpId = Option("5512f9dd2c41254c05e11dcb")
        val _accessLvl = AUTH_DEFAULT_LVL + AUTH_CAL_CREATE_LVL
        val _newUser = false
        val _name = "Facebook Test"
        runMe(_userid, _name, _type, _accessLvl, _cpId, _newUser)
      }

      case 3 => {
        //user that is not registered as admin
        val _userid = "114852108498604797792"
        val _type = "F"
        val _accessLvl = AUTH_DEFAULT_LVL
        val _cpId = Option.empty
        val _newUser = false
        runMe(_userid, _name, _type, _accessLvl, _cpId, _newUser)
      }

      case 4 => {
        //1000 random users
        for (counter <- 1 to 1000) {
          val _userid = "1148521084" + counter
          val _type = "F"
          val _accessLvl = AUTH_CAL_CREATE_LVL + AUTH_DEFAULT_LVL
          val _cpId = Option("550374462c4125ab0049afc6")
          val _newUser = false
          runMe(_userid, _name, _type, _accessLvl, _cpId, _newUser)
          createProfile(_userid, _type)
        }
        Ok("Done")
      }

      case 5 => {
        //create a user with admin
        val _userid = "10152559145073139"
        val _name = "Han Yoong"
        val _type = "F"
        val _accessLvl = AUTH_DEFAULT_LVL
        val _cpId = Option.empty
        val _newUser = true
        runMe(_userid, _name, _type, _accessLvl, _cpId, _newUser)
      }

      case 6 => {
        //create a user with admin
        val _userid = "10152559145073118"
        val _name = "Test Admin"
        val _type = "G"
        val _accessLvl = AUTH_DEFAULT_LVL
        val _cpId = Option.empty
        val _newUser = false
        createProfile(_userid, _type)
        runMe(_userid, _name, _type, _accessLvl, _cpId, _newUser)
      }
    }
  }

  private def createProfile(userId:String, oType:String){
    def profileCollection: JSONCollection = db.collection[JSONCollection](PROFILE.toString())
    val userProfile:UserProfileWithId = UserProfileWithId(
        oType+userId,
        "Test",
      	Option("Test"),
      	"Test",
      	"O",
      	"MY",
	      Option.empty,
	      Option.empty,
	      Option.empty,
	      Option.empty,
	      "KL"
        )
        try{
        	profileCollection.insert(userProfile)
        }catch{
        case e:Exception => //do nothing
        }
  }

  private def runMe(user_id:String, name:String, o_type:String, access_Lvl:Int, cp_Id:Option[String], new_User:Boolean):Result = {
    def userCollection: JSONCollection = db.collection[JSONCollection](USER.toString())

    val id = userIDCombination(o_type, user_id)
    val update = Json.obj( "newUser" -> JsBoolean(new_User), "authLevel" -> access_Lvl, "name" -> name)
    val con_jsonObj = if(cp_Id.isDefined){
      Json.obj("$set" -> update.++(Json.obj("cpId"->cp_Id.get)))
    } else Json.obj("$set" -> update).++(Json.obj("$unset"->Json.obj("cpId"->"")))

    userCollection.update(Json.obj("id"->user_id , "otype"-> o_type), con_jsonObj, GetLastError(), upsert = true, multi = false)

    val key = SecurityKey.getRandKey
    val publicKey = SecurityKey.encode(id, key)

    insertPassIntoDB(id, key, cp_Id.getOrElse(""), access_Lvl)
    Redirect(controllers.service.rest.routes.UserController.userInfo()).
      withSession((USER_ID, user_id),(OTYPE, o_type),(PUBLIC_KEY, publicKey),(CP_ID, cp_Id.getOrElse("")),(ACCESS_LVL, String.valueOf(access_Lvl)))

  }

  def prepareSettingEnv()= TestAction {
    def subscriptionCollection: JSONCollection = db.collection[JSONCollection](SUBSCRIPTION.toString())

    val cName = "Demo"
    val cDesc = "Demo"
    val cCtcNo = "999"
    val cWebsite = "None"
    val cEmail = "sample@gmail.com"
    val userId = "10152559145073139"
    val oType = "F"

    //remove subscription from database.
    val futureRemoval = subscriptionCollection.remove(Json.obj("cName" -> cName, "cDesc" -> cDesc));
    futureRemoval.onComplete {
      case Success(s) => {
        val createSub = Subscription(
          cName,
          cDesc,
          Option.apply(cCtcNo),
          Option.apply(cWebsite),
          Option.apply(cEmail),
          1
        )
        createSubscription(userId, oType, AUTH_DEFAULT_LVL, createSub)
      }
      case Failure(fail) => {
        System.err.println("Failed")
      }
    }
    Thread.sleep(1.seconds.toMillis)
    Ok("Done")
  }

  def prepareBookingEnv(confirm:Boolean)= TestAction {
    bookingEnv(confirm)
    Ok("Done")
  }

  private def bookingEnv(confirm:Boolean){
    def subscriptionCollection: JSONCollection = db.collection[JSONCollection](SUBSCRIPTION.toString())

      val cName = "Booking"
      val cDesc = "Booking"
      val cCtcNo = "999"
      val cWebsite = "None"
      val cEmail = "sample@gmail.com"
      val userId = "10152559145073118"
      val oType = "G"
      val authUserId = (oType+userId)

      //remove subscription from database.
      val futureSubRemoval = subscriptionCollection.remove(Json.obj("cName" -> cName, "cDesc" -> cDesc));
      futureSubRemoval.onComplete {
        case Success(s) => {
          val createSub = Subscription(
            cName,
            cDesc,
            Option.apply(cCtcNo),
            Option.apply(cWebsite),
            Option.apply(cEmail),
            1
          )
          createSubscription(userId, oType, AUTH_DEFAULT_LVL, createSub)
        }
        case Failure(fail) => {
          System.err.println("Failed")
        }
      }

      Thread.sleep(2.seconds.toMillis)

      def calendarCollection: JSONCollection = db.collection[JSONCollection](CALENDAR.toString())
      def userCollection: JSONCollection = db.collection[JSONCollection](USER.toString())
      val futureCalRemoval = calendarCollection.remove(Json.obj("userId" -> (oType+userId)));
      futureCalRemoval.onComplete{
        case Success(s) => {
          val query = Json.obj("otype" -> oType, "id" -> userId )
          val cursor:Cursor[User] = userCollection.find(query).cursor[User]
          val futureUserList: Future[List[User]] = cursor.collect[List]()
          futureUserList.map{ users =>
          	val currDate = (new DateTime(DateTimeZone.UTC)).plusHours(24)
          	val cal = new CalendarWithoutId("Sample Booking" , currDate.getMillis() , currDate.getMillis() , true , "Sample Booking" , 25, 5, confirm, authUserId, users(0).cpId.get )
          	val insRec = calendarCollection.insert(cal)
          }
        }
        case Failure(fail) => {
          System.err.println("Failed")
        }
      }

      Thread.sleep(1.seconds.toMillis)
  }
}
