package controllers.jobs

import akka.actor._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.libs.Akka
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.duration._
import models.beans.CalendarModel._
import io.lamma._
import org.joda.time.DateTime
import play.modules.reactivemongo.json.collection.JSONCollection
import play.Logger
import scala.util.Success
import scala.util.Failure
import org.joda.time.DateTimeZone
import play.api.libs.iteratee.Enumerator
import utils.ConfigurationSetup
import models.beans.EnumTableList._
import controllers.jobs.LogActor._
import utils.ZipReader
import scala.util.Failure
import java.io.File

object TimweReportJob {
	private val authSingleCall: ActorRef = Akka.system().actorOf(Props[BGReportJob], name="BGReportJob")  
  
	implicit val timeout = Timeout(2.minutes)
	
	def executeJob(fileName:String){
	    val status = authSingleCall ? (ReportJob(fileName))
	    status.onComplete(status =>
	      status match{
	        case Failure(f) => {
	        						println("Failed"); 
	        						try{
	        						  new File(ConfigurationSetup.FOLDER_TEMP + ConfigurationSetup.FOLDER_SPECIAL + fileName).delete()
	        						}catch{
	        							case e:Exception => e.printStackTrace() 
	        						}
	        					}
	        case Success(s) => {
	        						println("Completed"); 
	        						
	        					}
	      })
	}
}

class BGReportJob extends Actor{
  def receive() = {
    case ReportJob(fileName) =>{
      new ZipReader(fileName).process
    }
  }
}

case class ReportJob(fileName:String)
