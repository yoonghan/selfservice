package controllers.jobs

import akka.actor.UntypedActor
import play.api.Logger
import play.libs.Akka
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import akka.actor.ActorRef
import akka.actor.Props
import java.util.concurrent.TimeUnit
import com.jaring.jom.util.email.EmailUtility
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import reactivemongo.api.collections.default.BSONCollection
import scala.util.Success
import scala.util.Failure
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import play.api.libs.iteratee.Enumerator
import reactivemongo.api.Cursor
import models.beans.EnumTableList._
import models.beans.CalendarModel.CalendarRegisteredUser
import models.beans.ReminderModel.ReminderSetting
import models.beans.EmailNotifyModel.{EmailNotify,Notify}
import controllers.service.CommonKeys.EMAIL_REMINDER_TYPE 

 
object CalendarNotifyJob {
  
  val HOUR_PERIOD = 6;
  
  def init(){
    Logger.info("Booking notifier started")
    DateTimeZone.setDefault(DateTimeZone.UTC);
  }
  
  //Scan booking for every 1 hour only.
  val calendarNotifyActor:ActorRef = Akka.system().actorOf(Props[CalendarNotifyActor], name="CalendarNotifyActor");
    Akka.system().scheduler.schedule(
        Duration.create(0, TimeUnit.MILLISECONDS), 
        Duration.create(HOUR_PERIOD, TimeUnit.HOURS),
        calendarNotifyActor, "")
  
}

/**
 * 1. Check all the available dates first within the period and have subscribers
 * 2. Check all the users that have subscribed to the notification
 * 3. Email to them
 */
class CalendarNotifyActor extends UntypedActor {
  val db = ReactiveMongoPlugin.db
  val calCollection: JSONCollection = db.collection[JSONCollection](CALENDAR.toString())
  val reminderCollection: JSONCollection = db.collection[JSONCollection](REMINDER.toString())
  val notifCollection: JSONCollection = db.collection[JSONCollection](CALENDAR_NOTIFY.toString())
  val emailCollection: JSONCollection = db.collection[JSONCollection](EMAIL_NOTIFY_LIST.toString())
  
  //Depending on our period of job check in days.
  val notify_periods = Array(1,7);
  val dateFormat = "dd/MM/yyyy";
	
  override def onReceive(msg:Any){
    val currDate = (new DateTime(DateTimeZone.UTC))
    val resetDateWithoutHours = new DateTime(currDate.getYear(), currDate.getMonthOfYear(), currDate.getDayOfMonth(), 0, 0, 0);
    val dateSearch = resetDateWithoutHours.toString(dateFormat);
    
    //TODO: Improve searching by running query once
    for(period <- notify_periods){
    	val query = Json.obj("date"->dateSearch, "period"->period)
	    val searchQuery:Cursor[Notify] = notifCollection.find(query).cursor[Notify]
		val curFutureSubList: Future[List[Notify]] = searchQuery.collect[List]()
		
		curFutureSubList.map(notify =>
		  notify.size match{
		    case 0 => periodSearch(dateSearch, period)
		  }
		)  
	    
    }
  }
  
  /**
   * Check all the dates within the period that needs to be notified
   */
  def periodSearch(dateSearch:String, period:Int){
    val searchStartDate = (new DateTime(DateTimeZone.UTC)).plusDays(period-1);
    val searchEndDate = (new DateTime(DateTimeZone.UTC)).plusDays(period).minusMinutes(1);
    
    val query = Json.obj(
        "start" -> Json.obj("$gte" -> searchStartDate.getMillis()), 
        "start" -> Json.obj("$lte" -> searchEndDate.getMillis()), 
        "reg"-> Json.obj("$exists" -> true, "$not" -> Json.obj("$size" -> 0))
        )
	val searchQuery:Cursor[CalendarRegisteredUser] = calCollection.find(query).cursor[CalendarRegisteredUser]
	val curFutureSubList: Future[List[CalendarRegisteredUser]] = searchQuery.collect[List]()
	curFutureSubList.map(cal =>
	  searchUserRegistered(dateSearch, cal, period)
	)
  }
  
  /**
   * Search all users that is registered under the calendar setup
   */
  def searchUserRegistered(dateSearch:String, calList:List[CalendarRegisteredUser], period:Int){
    
    for(cal <- calList){
      for(userRegistered <- cal.reg.get){
        val query = Json.obj(
            "_id" -> userRegistered,
            "alertEmail" -> Json.obj("$exists" -> true, "$ne" -> ""),
            "reminderDays" -> Json.obj("$in" -> Json.arr(period)),
            "validEmail" -> true
            )
        val searchQuery:Cursor[ReminderSetting] = reminderCollection.find(query).cursor[ReminderSetting]    
        
        val curFutureSubList: Future[List[ReminderSetting]] = searchQuery.collect[List]()
        
        curFutureSubList.map(usersReminder =>
		  notifyViaEmail(dateSearch, usersReminder, cal)
		)
      }
    }
    
    updateCompletion(dateSearch, period);
  }
  
  /**
   * Prepare into database the email that needs to be sent
   */
  def notifyViaEmail(dateSearch:String, usersReminder:List[ReminderSetting], cal:CalendarRegisteredUser){
    val emailDistribution = (for(user <- usersReminder) yield (user.alertEmail.get)).mkString(",")
    val startDate = new DateTime(cal.start)
    val endDate = new DateTime(cal.end)
    val date = startDate.toString("EEE - dd/MMM/yyyy")
    val time = if(cal.allDay) {"Whole day"} else {startDate.toString("HH:mm a")}
    val title = cal.title 
    val desc = cal.desc.replaceAll("\n", " ") 
    val emailMessage = "Hello there,\n\n" +
    		"This is a kind reminder from JOM Jaring.\n\n" +
    		"You had made a booking and this are the provided details.\n"+
    		s"Date: $date\n"+
    		s"Time: $time\n"+
    		s"Title: $title\n"+
    		s"Desc: $desc\n\n"+
    		"Your punctuality against the time is important to/as our guest. Thank you.\n\n"+
    		"Reminder: Beware of fraudelant emails. We from JOM Jaring do not imply any charges from you for this service is provided free.\n\n"+
    		"Sincerity from,\n"+
    		"JOM Jaring";
    
    val _id = cal.id+dateSearch
    val notifyObj = EmailNotify(_id, cal.id, dateSearch, EMAIL_REMINDER_TYPE  ,emailDistribution, emailMessage, false)
    val insRec = emailCollection.insert(notifyObj)
    insRec.map{
        result =>
        if(!result.ok){
          LogActor.logActor ! ("notifyViaEmail >> Unable to insert:"+notifyObj.toString())
        }
    }
  }
  
  /**
   * Update database that that period have been handled
   */
  def updateCompletion(dateSearch:String, period:Int){
    val notifyObj = Notify(dateSearch , period)
    val insRec = notifCollection.insert(notifyObj)
    insRec.map{
        result =>
        if(!result.ok){
          LogActor.logActor ! ("updateCompletion >> Unable to insert:"+notifyObj.toString())
        }
    }
  }
}