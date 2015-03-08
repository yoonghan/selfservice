package models.beans

import play.api.libs.json._
import reactivemongo.bson._
import play.api.data.Forms.{mapping, longNumber, nonEmptyText, text, optional, boolean, list, number}
import play.api.data.FormError
import controllers.service.FormValidator
import scala.util.{Either,Left,Right}
import play.api.Logger
import models.beans.UserModel.UserStorageModel 


object CalendarModel {
  
  val EVENT_TYPE="opt1"
  val OCCURRENCE_TYPE="opt2"
  
  case class CalCmdReserve(id:String, userId:String)
  case class CalendarReserved(title:String, start:Long, end: Long, allDay: Boolean, desc: String, id:String)//need not to request excessive info
  case class Calendar(title:String, start:Long, end:Long, allDay:Boolean, desc:String, avail:Int, userInfo:Int, conf:Boolean, id:String)
  case class CalendarRegisteredUser(title:String, start:Long, end: Long, allDay: Boolean, desc: String, reg:Option[List[UserStorageModel]], pend:Option[List[UserStorageModel]], avail:Int, id:String)
  case class CalendarWithoutId(title:String, start:Long, end: Long, allDay: Boolean, desc: String, userInfo:Int, avail: Int, conf:Boolean, userId:String, cpId:String)
  case class TempCalendar(title:String, start:Long, end: Long, allDay: Boolean, desc: String, userInfo:Int, avail:Int, conf:Boolean, userId:String, virtualDel:Boolean, total:Int, count:Int)
  case class CalReservation(
      id:String,
      conf:Boolean,
      userInfo: Int,
      state: Option[String],
      pstCd: Option[String],
      addr: Option[String],
	  email: Option[String],
	  ctcNo: Option[String]
      )
  case class CalCancellation(
      id:String
      )
  case class TimedEvents(stime:String, etime:String, abookings: Int)
  case class ReservationSetup(
      title:String, 
      desc:String,
      userInfo: Int,
      fullDay:Boolean,
      conf:Option[Boolean],
      reserveType:String, 
      timedEvents:List[TimedEvents],
      occurrence:List[Boolean],
      reserveEvents:List[Long],
      blackoutEvents:List[Long]
      )

  def mongoReads[T](r: Reads[T]) = {
    __.json.update((__ \ 'id).json.copyFrom((__ \ '_id \ '$oid).json.pick[JsString] )) andThen r
  }
 
  def mongoWrites[T](w : Writes[T]) = {
    w.transform( js => js.as[JsObject] - "id"  ++ Json.obj("_id" -> Json.obj("$oid" -> js \ "id")) )
  }
 
  implicit val calRead: Reads[Calendar] = mongoReads[Calendar](Json.reads[Calendar])
  implicit val calWrites: Writes[Calendar] = mongoWrites[Calendar](Json.writes[Calendar])
  implicit val calResvRead: Reads[CalendarReserved] = mongoReads[CalendarReserved](Json.reads[CalendarReserved])
  implicit val calResvWrites: Writes[CalendarReserved] = mongoWrites[CalendarReserved](Json.writes[CalendarReserved])
  
  implicit val calReserveRead: Reads[CalReservation] = mongoReads[CalReservation](Json.reads[CalReservation])
  implicit val calReserveWrite: Writes[CalReservation] = mongoWrites[CalReservation](Json.writes[CalReservation])
  implicit val calCancellationRead: Reads[CalCancellation] = mongoReads[CalCancellation](Json.reads[CalCancellation])
  implicit val calCancellationWrite: Writes[CalCancellation] = mongoWrites[CalCancellation](Json.writes[CalCancellation])
  implicit val calUnRegRead: Reads[CalCmdReserve] = mongoReads[CalCmdReserve](Json.reads[CalCmdReserve])
  implicit val calUnRegWrite: Writes[CalCmdReserve] = mongoWrites[CalCmdReserve](Json.writes[CalCmdReserve])
  
  implicit val registeredUserRead: Reads[CalendarRegisteredUser] = mongoReads[CalendarRegisteredUser](Json.reads[CalendarRegisteredUser])
  implicit val registeredUserWrite: Writes[CalendarRegisteredUser] = mongoWrites[CalendarRegisteredUser](Json.writes[CalendarRegisteredUser])
    
  implicit val timedEventsRW = Json.format[TimedEvents]
  implicit val reservationSetupRW = Json.format[ReservationSetup]
  implicit val calendarWithoutIdRW = Json.format[CalendarWithoutId]
  implicit val tempCalendarRW = Json.format[TempCalendar]
  
  private val validateSetupForm = mapping(
      "title" -> nonEmptyText(3,30),
      "desc" -> nonEmptyText(3,300),
      "userInfo" -> number,
      "fullDay" -> boolean,
      "confReq" -> optional(boolean),
      "reserveType" -> nonEmptyText(1,4),
      "timedEvents" -> list(mapping(
          "stime" -> nonEmptyText,
          "etime" -> nonEmptyText,
          "abookings" -> number(1,99)
          )(TimedEvents.apply)(TimedEvents.unapply)),	
      "occurrence" -> list(boolean),
      "reserveEvents" -> list(longNumber),
      "blackoutEvents" -> list(longNumber)
  )(ReservationSetup.apply)(ReservationSetup.unapply)
  .verifying("Non full day must have timed events",
      fields => fields match {
        case reserveData => {(reserveData.fullDay == false && reserveData.timedEvents.size == 0) == false;}
      })
  .verifying("Specific occurrence must not have empty dates",
      fields => fields match {
        case reserveData => {((reserveData.reserveType == EVENT_TYPE && reserveData.reserveEvents.size == 0) || 
        					(reserveData.reserveType == OCCURRENCE_TYPE && !reserveData.occurrence(0) && !reserveData.occurrence(1)  && !reserveData.occurrence(2) && !reserveData.occurrence(3) && !reserveData.occurrence(4) && !reserveData.occurrence(5) && !reserveData.occurrence(6))) == false;}
      })
  .verifying("Some of the time set are invalid. Check end time against start time",
      fields => fields match {
        case reserveData =>
          if(reserveData.fullDay == false){
        	  val findInvalid = reserveData.timedEvents.filter( rec => (rec.stime >= rec.etime) ); findInvalid.size == 0;
          }else{
            true;
          }
      })
  .verifying("Maximum number of blackout dates(50), occurrences(50) and time(16) permitted allowed",
      fields => fields match {
        case reserveData => {
          (reserveData.fullDay == false && reserveData.timedEvents.size > 16 ||
              reserveData.reserveType == EVENT_TYPE && reserveData.reserveEvents.size > 50 ||
              reserveData.reserveType == OCCURRENCE_TYPE && reserveData.timedEvents.size > 50
          )==false
        }
      })
      
  def matchField(field:String):String = {
     field match {
       case "id" => "ID"
       case _ => field
     }
  }
  
  def matchSetupField(field:String):String = {
     field match {
       case "title" => "Title"
       case "desc" => "Description"
       case "fullDay" => "Full Day"
       case "confReq" => "Confirmation Required"
       case "reserveType" => "Reserve Type"
       case "timedEvents.startTime" => "Start Time"
       case "timedEvents.endTime" => "End Time"
       case "timedEvents.allowBooking" => "Allowed Booking"
       case "occurrence" => "Occurrence"
       case "reserveEvents" => "Reserve Events"
       case "blackoutEvents" => "Blackout Events"
       case "userInfo" => "User Info"
       case _ => field
     }
  }
  
    def validateSetupInput(reservationReq: CalReservation):List[String] = {
      import com.jaring.jom.util.converter.NumberConverter._
      import models.beans.UserModel._
      
      val ternary:String = reservationReq.userInfo.toTernaryString
      val ternaryLength = ternary.length()
      
      def condition(value:Option[String], pos:Int):Either[Option[String], String] = {
        
        if(pos > ternaryLength)
          if(value.isEmpty) Left(value) else Right(" invalid key submitted")
        else{
          val opt = ternary.charAt(ternaryLength - pos)
          opt match{
            case '0' => if(value.isEmpty) {Left(value)} else Right(" must be empty")	//do not provide
            case '1' =>	Left(value) //optional
            case '2' => if(value.isEmpty) Right(" is Mandatory") else Left(value) //mandatory
            case _ => Right(" invalid key")	//will never happen
          }
        }
      }
      
      import controllers.service.CommonKeys._
      val toValidate = Map("pstCd"->condition(reservationReq.pstCd, TERNARY_POS_ADDRESS), 
    		  				"addr"->condition(reservationReq.addr, TERNARY_POS_ADDRESS),
    		  				"state"->condition(reservationReq.state, TERNARY_POS_ADDRESS),
    		  				"email"->condition(reservationReq.email, TERNARY_POS_EMAIL),
    		  				"ctcNo"->condition(reservationReq.ctcNo, TERNARY_POS_CONTACTNO))
    		  				
      val errorList = toValidate.filter(_._2.isRight)
      val errors = errorList.map(mapField => UserModel.matchField(mapField._1) + mapField._2.right.getOrElse(" unknown error"))
          
      if(errors.isEmpty){
        val userProfile = UserProfile(
	      "Sample", //Something valid
	      Option.empty, 
	      "Sample", //Something valid
	      "O",//Something valid
	      "MY",//Something valid
	      toValidate.get("pstCd").get.left.get, 
	      toValidate.get("addr").get.left.get,
	      toValidate.get("email").get.left.get,
	      toValidate.get("ctcNo").get.left.get,
	      toValidate.get("state").get.left.get.getOrElse("KL")
	      )
	      validateInput(userProfile)
      }else{
        errors.toList
      }
    }
  
  def validateSetupInput(reservationSetup: ReservationSetup):List[String] = {
    
    val blackoutEvents = for( l <- 0 until reservationSetup.blackoutEvents.length) yield {
      "blackoutEvents["+l+"]" -> String.valueOf(reservationSetup.blackoutEvents(l))
    }
    val reserveEvents = for( l <- 0 until reservationSetup.reserveEvents.length) yield {
      "reserveEvents["+l+"]" -> String.valueOf(reservationSetup.reserveEvents(l))
    }
    val occurrences = for( l <- 0 until reservationSetup.occurrence.length) yield {
      "occurrence["+l+"]" -> String.valueOf(reservationSetup.occurrence(l))
    }
    val stime = for( l <- 0 until reservationSetup.timedEvents.length) yield {
      "timedEvents["+l+"].stime" -> reservationSetup.timedEvents(l).stime
    }
    val etime = for( l <- 0 until reservationSetup.timedEvents.length) yield {
      "timedEvents["+l+"].etime" -> reservationSetup.timedEvents(l).etime
    }
    val abookings = for( l <- 0 until reservationSetup.timedEvents.length) yield {
      "timedEvents["+l+"].abookings" -> String.valueOf(reservationSetup.timedEvents(l).abookings)
    }
    val map = Map(
        "title" -> reservationSetup.title,
    	"desc" -> reservationSetup.desc,
    	"userInfo" -> reservationSetup.userInfo.toString,
    	"fullDay" -> String.valueOf(reservationSetup.fullDay),
    	"reserveType" -> String.valueOf(reservationSetup.reserveType)
    	)
    	
    val fullmap = map ++ blackoutEvents.toMap ++ reserveEvents.toMap ++ occurrences.toMap ++ stime.toMap ++ etime.toMap ++ abookings.toMap;
    
    val validForm = validateSetupForm.bind(
        fullmap
      )
     FormValidator.validForm(validForm, matchSetupField);
  }
  
   def toReservationError(errorList:List[String]) = {
    val errorArray = errorList.foldLeft(JsArray())((acc, x) => acc ++ Json.arr(x))
    Json.obj(
      "errors" -> errorArray
    )
  }
   
   
}
