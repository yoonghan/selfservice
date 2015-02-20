package models.auth

import com.jaring.jom.util.authentication._
import com.jaring.jom.util.impl._
import controllers.service.CommonKeys._

object OAuth2Authentication{
  
  val GOOGLE:String = "G"
  val DEV_GOOGLE:String = "D_G"
  val FACEBOOK:String = "F"
  val DEV_FACEBOOK:String = "D_F"
  val TOKEN_KEY:String = "state"
  val CALLBACK_URL:String = play.api.Play.current.configuration.getString("oauth2.callbackURL").getOrElse("localhost")
  val REDIRECT_EXISTING_USER_URL:String = play.api.Play.current.configuration.getString("oauth2.callbackExistRedirectURL").getOrElse("localhost")
  val REDIRECT_NEW_USER_URL:String = play.api.Play.current.configuration.getString("oauth2.callbackNewRedirectURL").getOrElse("localhost")
	
  val facebookAuthenticate = new FacebookAuthentication(CALLBACK_URL);
  val googleAuthenticate = new GoogleAuthentication(CALLBACK_URL);
  
  def getOAuth(aType:String):Option[IOAuthImpl] = { 
    
	aType match {
		case GOOGLE | DEV_GOOGLE=> Option.apply(googleAuthenticate);
		case FACEBOOK | DEV_FACEBOOK => Option.apply(facebookAuthenticate);
		case _ => Option.empty[IOAuthImpl]; 
	}
  }
}