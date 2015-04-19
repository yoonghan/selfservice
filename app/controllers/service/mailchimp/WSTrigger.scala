package controllers.service.mailchimp

import play.api.libs.ws._
import scala.concurrent.Future
import utils.ConfigurationSetup
import play.api.mvc.Controller
import play.api.libs.json._
import play.api.Play.current
import com.jaring.jom.util.common.PropertyLoaderUtil
import java.util.Properties

object WSTrigger{

  //Timeout duration has been configured in application.conf
  
  val property:Properties = PropertyLoaderUtil.propertyLoader("mailchimp.properties").get
  val URL = property.getProperty("mailchimp.url")
  val APIKEY = property.getProperty("mailchimp.apikey")
  property.clear();
  
  private def getWS(relativePath:String):WSRequestHolder = {
	  WS.url(URL + relativePath)
			.withHeaders("content-type" -> "application/json")
  }
		  						
  def ping():Future[Response] = {
    val path = "helper/ping"
    val data = Json.obj("apikey"->APIKEY)
    getWS(path).post(data)
  }
}