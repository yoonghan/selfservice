package controllers.jobs

import akka.actor._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.libs.Akka
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.duration._
import models.beans.CalendarModel._
import io.lamma._
import org.joda.time.DateTime
import play.modules.reactivemongo.json.collection.JSONCollection
import play.Logger
import scala.util.Success
import scala.util.Failure
import org.joda.time.DateTimeZone
import play.api.libs.iteratee.Enumerator
import utils.ConfigurationSetup
import models.beans.EnumTableList._
import controllers.jobs.LogActor._

object CalendarCreator{
  private val authSingleCall: ActorRef = Akka.system().actorOf(Props[BGCalendarCreator], name="BGCalendarCreator")  
  //Make sure all timezone is in UTC
  DateTimeZone.setDefault(DateTimeZone.UTC);
  
  implicit val timeout = Timeout(30.seconds)
  
  def createCalendar(calendar:ReservationSetup, userId:String){
    authSingleCall ? (new CalendarCreation(calendar,userId))
  }
  
  def makeLiveCalendar(calendar:List[TempCalendar], cpId:String, userId:String){
    authSingleCall ? (new CopyCalendar(calendar,cpId, userId))
  }
  
  def removeLiveCalendar(userId:String){
    authSingleCall ? (new RemoveCalendar(userId))
  }
}

class BGCalendarCreator extends Actor with MongoJob{

  def tmpCalCollection: JSONCollection = db.collection[JSONCollection](CALENDAR_TEMP.toString())
  def calCollection: JSONCollection = db.collection[JSONCollection](CALENDAR.toString())
  def subCollection: JSONCollection = db.collection[JSONCollection](SUBSCRIPTION.toString());

	def testOccurrence(rs:ReservationSetup, userId:String){
	    val returnList = createListOfOccurrences(rs.reserveType, rs.occurrence, rs.reserveEvents )
	    returnList.foreach(f=>println("-->"+new DateTime(f)))
  	}
  
	private def createListOfOccurrences(rsType:String, occurrence:List[Boolean], events:List[Long]):List[Long] = {
	  
	  val currDate = (new DateTime(DateTimeZone.UTC)).plusHours(ConfigurationSetup.MIN_BOOKING_HR);
	  
	  val calList = rsType match {
	    case OCCURRENCE_TYPE =>{
	      if(occurrence.length == 7){
	        
	        val weekValues = Array(DayOfWeek.MONDAY, DayOfWeek.TUESDAY, DayOfWeek.WEDNESDAY, DayOfWeek.THURSDAY, DayOfWeek.FRIDAY, DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)
	        
		
	        val calculatedDates = for( occur <- 0 until occurrence.length if occurrence(occur)) yield {
	          
	    	  val javaList = Dates.from(currDate.getYear(), currDate.getMonthOfYear(), currDate.getDayOfMonth()).
	    	  to(currDate.getYear()+1, currDate.getMonthOfYear(), currDate.getDayOfMonth()).
	    	  byWeek().on(weekValues(occur)).build();
	    	  
	    	  val newList = for(p <- 0 until javaList.size()) yield {
	    	    new DateTime(javaList.get(p).yyyy.toInt, javaList.get(p).mm.toInt, javaList.get(p).dd.toInt, 0, 0, 0).getMillis()
	    	  }
	    	  
	    	  newList
	        }
	    	
	        (calculatedDates.flatten).toList.sorted
	        
	      }else{
	    	logActor ! "Created something out of the box:"+occurrence.toString
	        Nil
	      }
	    } 
	    case EVENT_TYPE =>{
	      events.filter(dates => dates > currDate.getMillis())
	    }
	  }
	  
	  calList
	}
	
	private def blackOutListOfOccurrences(listOfDates:List[Long], blackoutDates:List[Long]) = {
	  //refactor all blackout dates without time.
	  val emptyBlackoutDates = blackoutDates.map{ dt => 
	      val newDt = new DateTime(dt);
	      val dateInFormat = new DateTime(newDt.getYear(), newDt.getMonthOfYear(), newDt.getDayOfMonth(), 0, 0, 0)
	      dateInFormat.getMillis()
	  };
	  
	  listOfDates.filterNot( emptyBlackoutDates.contains( _ ))
	}
	
	private def generateData(listOfDates:List[Long], fullDay:Boolean, timedEvents:List[TimedEvents], title:String, desc:String, userInfo:Int, confReq:Boolean, userId:String ) = {
	  
		val events = if(timedEvents.length == 0){
		  List(new TimedEvents("0001","0100",1))
		}else{
		  timedEvents
		}
		
		val timeList = for(event <- events) yield{
			val sHr = try{Integer.parseInt(event.stime.substring(0,2),10)}catch{case e:Exception=> 0}
		    val sMin = try{Integer.parseInt(event.stime.substring(2),10)}catch{case e:Exception=> 0}
		    val eHr = try{Integer.parseInt(event.etime.substring(0,2),10)}catch{case e:Exception=> 0}
		    val eMin = try{Integer.parseInt(event.etime.substring(2),10)}catch{case e:Exception=> 0}
		    
		    (sHr, sMin, eHr, eMin, event.abookings)
		}
	  
		/**
		 * Corrected all Dates with time
		 */
		case class tmp_TempCalendar(start:Long, end: Long, avail:Int)
		
		val allList = for( 
	      counter <- listOfDates.length-1 to 0 by -1;
	      subCounter <- timeList.length-1 to 0 by -1
		  ) yield {
		    val dt = new DateTime(listOfDates(counter))
		    
		    val event = timeList(subCounter)
		    
		    val dates = if(fullDay) {
		    				val sDt = new DateTime(dt.getYear(), dt.getMonthOfYear(), dt.getDayOfMonth(), 0, 1, 0)
		    				val eDt = new DateTime(dt.getYear(), dt.getMonthOfYear(), dt.getDayOfMonth(), 23, 0 , 0)
		    				(sDt, eDt) 
		    			}else {
		    				  val sDt = new DateTime(dt.getYear(), dt.getMonthOfYear(), dt.getDayOfMonth(), event._1, event._2 , 0)
		    				  val eDt = new DateTime(dt.getYear(), dt.getMonthOfYear(), dt.getDayOfMonth(), event._3, event._4 , 0)
		    				  (sDt, eDt)
		    			}
		    
		    val cal = new tmp_TempCalendar(dates._1.getMillis(), dates._2 .getMillis(), event._5)
		    
		    cal
		}
		
		/**
		 * Used for progress bar loading
		 */
		val total=allList.length
		val counterList = for(counter <- 1 to total) yield {
		  val getList = allList(counter - 1) 
		  val cal = new TempCalendar(title, getList.start , getList.end , fullDay, desc, userInfo, getList.avail, confReq, userId, false, total, counter)
		  cal
		}
	  
		counterList
	}
	
	private def insertToDatabase(data:IndexedSeq[TempCalendar], userId:String){
	    val SIZE = 50
	    
	    def listingSplitAndInsert(finishing:IndexedSeq[TempCalendar]){
	    	if(finishing.size == 0)
	    	  return;
	      
	    	val enumerator = Enumerator.enumerate(finishing.take(SIZE))
	    	val insRec = tmpCalCollection.bulkInsert(enumerator)
	    		
	    	insRec.onComplete{
	    	  case Success(x) => Logger.info("Completed bulk insert of:-"+finishing.size);
	    	  case Failure(failure) =>{
	    	    failure.printStackTrace();
	    	    logActor ! ("Insert with error: UserId:-"+userId+",failure:-"+failure.getMessage())
	    	  }
	    	}
	    	listingSplitAndInsert(finishing.drop(SIZE))
		}
	    
	    listingSplitAndInsert(data);
	}
	
	private def removeFromDatabase(userId:String){
    import reactivemongo.core.commands.GetLastError
    val query = Json.obj("userId"->userId);
		val updateRec = tmpCalCollection.remove(query, GetLastError(), false)
//		updateRec.map{
//          result => {
//            if(result.updated == 0)
//            	Logger.error("Nothing to be deleted");
//          }
//        }
	}
  
	
	/**
	 * Actor Begins here.
	 **/
	def receive() = {

//Creation Case
	  case CalendarCreation(rs,userId) =>{
	    val listOfDates = createListOfOccurrences(rs.reserveType, rs.occurrence, rs.reserveEvents)
	    
	    val filteredOffDates = blackOutListOfOccurrences(listOfDates, rs.blackoutEvents )
	    
	    val finishing = generateData(filteredOffDates, rs.fullDay , rs.timedEvents, rs.title, rs.desc, rs.userInfo, rs.conf.getOrElse(false), userId )
	    
	    removeFromDatabase(userId);
	    insertToDatabase(finishing, userId);
	    	
	    Logger.info("Completed receive of userId:-"+userId)
	  }
//Publishing Case
	  case CopyCalendar(rs, cpId, userId) =>{
	    for(each <- rs){
	      if(each.virtualDel == false){
	      val cal = new CalendarWithoutId(each.title , each.start , each.end , each.allDay , each.desc , each.userInfo, each.avail, each.conf, each.userId, cpId )
	      val insRec = calCollection.insert(cal)
	    	insRec.onComplete{
	    	  case Success(x) => ;
	    	  case Failure(failure) =>{
	    	    failure.printStackTrace();
	    	    Logger.error("Insert with error: CpId:-"+cpId+",failure:-"+failure.getMessage())
	    	  }
	    	}
	      }
	    }    
	    
	    import reactivemongo.core.commands.GetLastError
	    val query = Json.obj("userId"->userId);
		val updateRec = tmpCalCollection.remove(query, GetLastError(), false)
		updateRec.map{
          result => {
            if(result.updated == 0)
            	Logger.error("Nothing to be deleted");
          }
        }
	  }
	  
//Rejection instead of publishing
	  case RemoveCalendar(userId) =>{
	    removeFromDatabase(userId)
	  }
	  
	  case _ =>{
	    logActor ! "Invalid calendar call"
	  }
	    
	}
}

case class CalendarCreation(rs:ReservationSetup, userId:String);
case class CopyCalendar(rs:List[TempCalendar], cpId:String, userId:String);
case class RemoveCalendar(userId:String);