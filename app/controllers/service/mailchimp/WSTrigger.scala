package controllers.service.mailchimp

import play.api.libs.ws._
import scala.concurrent.Future
import utils.ConfigurationSetup
import play.api.mvc.Controller

import play.api.libs.json._
import play.api.Play.current

object WSTrigger{

  //Timeout duration has been configured in application.conf
  
  val APIKEY = ConfigurationSetup.MAILCHIMP_APIKEY 
		  						
  private def getWS(relativePath:String):WSRequestHolder = {
	  WS.url(ConfigurationSetup.MAILCHIMP_URL + relativePath)
			.withHeaders("content-type" -> "application/json")
  }
		  						
  def ping():Future[Response] = {
    val path = "helper/ping"
    val data = Json.obj("apikey"->APIKEY)
    getWS(path).post(data)
  }
}