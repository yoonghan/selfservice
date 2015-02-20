import play.Application
import play.GlobalSettings
import play.libs.Akka
import scala.concurrent.duration.Duration
import akka.actor.ActorRef
import akka.actor.Props
import java.util.concurrent.TimeUnit
import play.api.Logger
import controllers.jobs.{LogActor,CalendarNotifyJob,EmailSendJob}

/**Global play settings, must always be in root folder**/
 
class Global extends GlobalSettings {
      override def onStart(app:Application) {
        LogActor.init
        CalendarNotifyJob.init
        EmailSendJob.init
      }
}