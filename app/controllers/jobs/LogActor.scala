package controllers.jobs

import akka.actor.UntypedActor
import play.api.Logger
import play.libs.Akka
import scala.concurrent.duration.Duration
import akka.actor.ActorRef
import akka.actor.Props
import java.util.concurrent.TimeUnit
import com.jaring.jom.util.email.EmailUtility
import scala.concurrent.ExecutionContext.Implicits.global

 
object LogActor {
  
  def init(){
    Logger.info("Logger monitoring started")
  }
  
  //Ping only 6 hours once.
  val logActor:ActorRef = Akka.system().actorOf(Props[LogCheckJob], name="logActor");
    Akka.system().scheduler.schedule(
        Duration.create(0, TimeUnit.MILLISECONDS), 
        Duration.create(6, TimeUnit.HOURS),
        logActor, "")
  
}

class LogCheckJob extends UntypedActor {
 
    override def onReceive(msg:Any)={
    	//create this message as email and email it.
      val strMsg = msg.asInstanceOf[String]
      if( filter(strMsg)){
        controllers.service.Utility.sendEmail(msg.asInstanceOf[String])
      }
    }
    
    private def filter(msg:String):Boolean = {
      msg match{
        case "" => false
        case "Error Code :key not found: userId" => false
        case _ => true
      }
    }
}