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
import models.beans.ReminderModel.ReminderSetting
import models.beans.EmailNotifyModel.EmailNotify
import models.beans.EnumTableList.EMAIL_NOTIFY_LIST

object EmailSendJob {
  
  val MINS_PERIOD = 15;
  
  def init(){
    Logger.info("Email notifier started")
    DateTimeZone.setDefault(DateTimeZone.UTC);
  }
  
  //Scan email for every 15 minutes.
  val emailSendActor:ActorRef = Akka.system().actorOf(Props[EmailSendActor], name="emailSendActor");
    Akka.system().scheduler.schedule(
        Duration.create(0, TimeUnit.MILLISECONDS), 
        Duration.create(MINS_PERIOD, TimeUnit.MINUTES),
        emailSendActor, "")
  
}

/**
 * Send email that have been inserted into emailNotifyList
 */
class EmailSendActor extends UntypedActor {
  val db = ReactiveMongoPlugin.db
  val MAX_LIMIT = 20	//max only 20 emails to be sent per cycle
  val emailCollection: JSONCollection = db.collection[JSONCollection](EMAIL_NOTIFY_LIST.toString())
  
  override def onReceive(msg:Any){
    val query = Json.obj()
    val searchQuery:Cursor[EmailNotify] = emailCollection.find(query).cursor[EmailNotify]
    val curFutureSubList: Future[List[EmailNotify]] = searchQuery.collect[List](MAX_LIMIT, true)
        
    curFutureSubList.map(emailsToSend =>
    	sendEmailsOut(emailsToSend)
	)
  }

  def sendEmailsOut(emailsToSend:List[EmailNotify]){
    emailsToSend.par.map{ email=>   
      
     controllers.service.Utility.sendEmail(Option(email.emailDist), "A Kind Reminder", email.message )
     
     val query = Json.obj("_id" -> email._id)
     val result = emailCollection.remove(query, reactivemongo.core.commands.GetLastError(), true)
     result.onFailure({
       case fail => LogActor.logActor ! ("Email sent unsuccessful:"+fail.getMessage())
     })
    }
  }
}