package controllers

import play.api._
import play.api.mvc._
import utils.ConfigurationSetup._


object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Sorry, this page is just a servicing page."))
  }
  
  def help = Action {
     Ok(views.html.help(""))
  }
  
  def ping = Action {
    Ok("Pong").withHeaders(
        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS))
  }
  
}