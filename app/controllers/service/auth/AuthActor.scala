package controllers.service.auth

import com.jaring.jom.util.authentication._
import akka.actor._
import play.api.libs.json._
import models.auth.OAuth2Authentication
import models.auth.OAuth2Authentication._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import play.libs.Akka
import akka.util.Timeout
import scala.util.Try
import akka.pattern.ask
import scala.concurrent.duration._
import utils.CommonKeys._

object Authorize{
  private val authAsync: ActorRef = Akka.system().actorOf(Props[AuthActor], name="Authorization")
  
  def authorize(state:String, authCode:String, sessState:String) : Future[Try[(String,String)]] = {
    implicit val timeout = Timeout(30.seconds)
    
    authAsync ?  Authenticate(state, authCode, sessState) map {
      case AuthenticateJSON(result) => result
    }
    
  }
}

class AuthActor extends Actor{ 

	private def isTokenValid(tokenParameter:String, authCode:String, sessState:String):Boolean = {
		tokenParameter != null && authCode != null && sessState != null &&
		!tokenParameter.isEmpty() && !authCode.isEmpty() && !sessState.isEmpty() &&
		tokenParameter == sessState
	}
    
	private def checkTokenKey(tokenKey:String):String ={
	  if(tokenKey.startsWith(DEVICE_APP)){
	    if (tokenKey.startsWith(DEVICE_APP+EnumAuthentication.FACEBOOK_TOKEN.toString()))
	      DEV_FACEBOOK
	    else if(tokenKey.startsWith(DEVICE_APP+EnumAuthentication.GOOGLE_TOKEN.toString()))
	      DEV_GOOGLE
	    else
	        ""
	  }else if (tokenKey.startsWith(EnumAuthentication.FACEBOOK_TOKEN.toString()))
	      FACEBOOK
	    else if(tokenKey.startsWith(EnumAuthentication.GOOGLE_TOKEN.toString()))
	      GOOGLE
	    else
	      ""
	}

	private def authorize(state:String, authCode:String, sessState:String):(String,String) = {
	  if(isTokenValid(state, authCode, sessState)){
		val otype = checkTokenKey(state);
	  	val json = OAuth2Authentication.getOAuth(otype)
		if(json.isDefined){
			val jsonVal = json.get.getUserInfoJson(authCode)
			(jsonVal.getOrElse("""{errMsg: "Not authorized"}"""),otype) 
		}else{
		  throw new IllegalAccessException("No such authorization")
		}
	  }else{
	    throw new IllegalAccessException("Invalid token")
	  }
	}
  
	def receive = {
	  case Authenticate(state, authCode, sessState) => {
	    
		val futureAuthorization = Future { authorize(state, authCode, sessState) }
		
	  	val client = sender
	  
	  	futureAuthorization.onComplete{
	  	  client ! AuthenticateJSON(_)
	  	}
	  }
	}

}

case class Authenticate(state:String, authCode:String, sessState:String)
case class AuthenticateJSON(jsonVal:Try[(String,String)])