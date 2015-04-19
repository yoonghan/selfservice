package controllers.service.mailchimp

import controllers.service.BaseApiController
import play.api.mvc.Action
import scala.concurrent.ExecutionContext.Implicits.global

object MailChimpController extends BaseApiController{

  def ping() = Action.async {
    WSTrigger.ping.map { response => 
      Ok("Feed title" + (response.json \ "msg").as[String])
    }
  }
}