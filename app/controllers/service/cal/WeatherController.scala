package controllers.service.cal

import play.api.mvc.Controller
import com.film42.forecastioapi.ForecastIO
import scala.util.Success
import play.modules.reactivemongo.MongoController
import play.api.Logger
import models.beans.WeatherModel._
import play.api.Play.current
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api._
import play.api.libs.json._
import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.WebSocket
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import play.api.libs.iteratee.Concurrent
import controllers.service.BaseApiController
import play.api.mvc.Action
import java.util.Date

/**
 * Weather controller is only available as web sockets
 */
object WeatherController extends BaseApiController with MongoController {
  def collection: JSONCollection = db.collection[JSONCollection](LOCATION.toString())
    
  def index = WebSocket.acceptWithActor[WeatherRequest, WeatherResponse] {request => out =>
    WeatherWebSocketActor.weather(out)
  }
  
  def weatherlocations = Action{
    val mapLocation = models.beans.WeatherModel.location
    val list = for(location <- mapLocation) yield JsObject("id" -> JsString(location._1) :: "location" -> JsString(location._2._1) :: Nil)
    JsonResponse(Ok(Json.obj("locationlist"->list.toList)), 3600)
  }
}

object WeatherWebSocketActor{
	def weather(out: ActorRef) = Props(new WeatherWebSocketActor(out))
}

class WeatherWebSocketActor(out: ActorRef) extends Actor{
	val forecastIO = ForecastIO(current.configuration.getString("forecast-io-api-key").get);
  
	
	def convertDate(date:String):Long = {
		try{
			java.lang.Long.parseLong(date, 10)//radix 10
		}catch{
		  case e:Exception => 0L
		}
	}
	
	def receive = {	  
	  case msg:WeatherRequest =>{
	    val (state, lat, long) = models.beans.WeatherModel.location.get(msg.state).get //default value is passed.
	    val convertedDate = convertDate(msg.date);
	    
	    val forecast = 
	    	if(convertedDate != 0) forecastIO.forecast(lat, long, new Date(convertedDate))
	    	else forecastIO.forecast(lat, long)
		if(forecast.isSuccess)
		  out ! new WeatherResponse(forecast.get.currently.icon, forecast.get.currently.summary, msg.date)
		else
		  out ! new WeatherResponse("unknown", "unknown", "unknown")
	  }
	  case _ => {
	    out ! new WeatherResponse("unknown", "unknown", "unknown")
	  }
	}
}