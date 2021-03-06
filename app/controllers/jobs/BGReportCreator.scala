package controllers.jobs

import akka.actor._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import play.libs.Akka
import akka.util.Timeout
import scala.util.{Try, Success, Failure}
import akka.pattern.ask
import scala.concurrent.duration._
import models.beans.CalendarModel._
import play.modules.reactivemongo.json.collection.JSONCollection
import play.Logger
import scala.concurrent.Await
import org.joda.time.DateTimeZone
import utils.ConfigurationSetup
import reactivemongo.api.Cursor
import controllers.jobs.LogActor._
import models.beans.EnumTableList._
import models.beans.UserModel._
import models.beans.SubscriptionModel._
import play.api.libs.Files._
import java.io.File
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import utils.ConfigurationSetup._
import java.io.FileOutputStream
import org.apache.poi.ss.util.WorkbookUtil
import org.apache.poi.xssf.usermodel.{XSSFSheet,XSSFCell,XSSFRow}
import models.beans.ReportModel.{ReportNotify,ReportNotifyWithoutId}
import reactivemongo.core.commands.GetLastError

object ReportCreator {
	private val reportSingleCall: ActorRef = Akka.system().actorOf(Props[BGReportCreator], name="BGReportCreator")

	DateTimeZone.setDefault(DateTimeZone.UTC);

	implicit val timeout = Timeout(60.seconds)	// allows a minute to create

	def createCalendar(corporateId:String) = {
	  val retVal = reportSingleCall ? (new ReportCreation(corporateId))
	}
}

class BGReportCreator extends Actor with MongoJob{

  val eventHeader = Array("Status", "First Name", "Last Name", "Email", "Contact No", "Address", "PostCode", "State")
  val corporateHeader = Array("Name", "Description", "Website", "Contact No", "Email")
  val eventListHeader = Array("Title", "Description", "Start Date Time", "End Date Time", "Availability Left", "Reserve Count")
  val eventGap = 6 //This is for the event to ensure that a gap is left

  def reportCollection: JSONCollection = db.collection[JSONCollection](REPORT_NOTIFY_LIST.toString())
  def calCollection: JSONCollection = db.collection[JSONCollection](CALENDAR.toString())
  def profileCollection: JSONCollection = db.collection[JSONCollection](PROFILE.toString())
  def subscriptionCollection: JSONCollection = db.collection[JSONCollection](SUBSCRIPTION.toString())

  /**
   * Print all system data, for future troubleshooting.
   */
	def printSysData(id:Long){
		Logger.info("Report Log ID:[" + id + "], Memory Avail >>> " +Runtime.getRuntime().freeMemory() + ", Time >>> " + System.currentTimeMillis())
	}

	/**
	 * Generate the excel.
	 */
	def genExcel(corporateId:String){
	  /**
	   * Get the list of events per corporate.
	   */
	  def getListOfEvents() = {
	    val query = Json.obj(
	        "cpId" -> corporateId
	        )
	    val sort = Json.obj("start" -> -1)
	    val cursor:Cursor[CalendarRegisteredUser] = calCollection.find(query).sort(sort).cursor[CalendarRegisteredUser]

	    val futureCalList: Future[List[CalendarRegisteredUser]] = cursor.collect[List]()
	    futureCalList
	  }

	  /**
	   * Process User details per Event.
	   */
	  def processUserPerEvent(regModel:List[UserStorageModel], pendModel:List[UserStorageModel],
	    calRegUser:CalendarRegisteredUser, startGap:Int, eventSheet:XSSFSheet, workbook:XSSFWorkbook):Future[List[UserProfileWithId]] = {

	    val regMap = regModel.map(t => t.id -> t).toMap
	    val pendMap = pendModel.map(t => t.id -> t).toMap

	    val query = Json.obj(
	        "_id" -> Json.obj("$in" -> (regModel.map(_.id) ++ pendModel.map(_.id)))
	        )
	    val cursor:Cursor[UserProfileWithId] = profileCollection.find(query).cursor[UserProfileWithId]
	    val futureUserList: Future[List[UserProfileWithId]] = cursor.collect[List]()

	    val rowCreator = createRow(eventSheet) _

	    futureUserList.map { userList =>
	      for(userCnt <- 0 until userList.size){
	        val row = rowCreator((startGap) + (userCnt))
	        val id = userList(userCnt)._id
	        val userModel = regMap.get(id).getOrElse(pendMap.get(id).get)

	        writeUser(regMap.contains(id), userList(userCnt).firstName , userList(userCnt).lastName ,userModel, row)
	      }
	    }

	    futureUserList
	  }

	  /**
	   * Create a temporary workbook
	   */
	  def prepareExcel() = {
      new XSSFWorkbook
	  }

	  /**
	   * Email attachment
	   */
	  def emailFile(sourceFile:String, fileName:String){
	    val message = "Hello there,\n\n" +
	    		"Attached is the report of the event calendar that you have had with us.\n\n" +
	    		"Reminder: Beware of fraudelant emails. We from Walcron do not imply any charges from you for this service is provided free.\n\n"+
	    		"Sincerity from,\n"+
	    		"Walcron Coorperation";
	    utils.Utility.sendEmail(Option.empty,
	        "Event Report",
	        message,
	        sourceFile,
					fileName)
	  }

	  /**
	   * Close excel
	   */
	  def closeExcel(wb:XSSFWorkbook) = {
	    val sourceFile = ConfigurationSetup.FOLDER_TEMP + "excel_" + corporateId + EXCEL_TYPE
			var file = new File(sourceFile)
	    val tempFile = new TemporaryFile(file)
	    val fileOutput = new FileOutputStream(tempFile.file)
	    try{
		    wb.write(fileOutput)
		    wb.close()

		    emailFile(sourceFile, file.getName())

		    fileOutput.close()
		    tempFile.clean
	    }catch{
	      case e:Exception =>{
	        e.printStackTrace()
	        logActor ! "File not able to close: "+sourceFile
	      }
	    }

	    removeFromDB();
    }

    def removeFromDB(){
      val query = Json.obj("_id"->corporateId);
      val updateRec = reportCollection.remove(query, GetLastError(), false)
      updateRec.map{
        result => {
          if(result.updated == 0)
            Logger.error("Nothing to be deleted");
        }
      }
    }

	  /**
	   * Create excel sheet.
	   */
	  def createSheet(workbook:XSSFWorkbook, sheetName:String):XSSFSheet = {
	    val renameSheetName = WorkbookUtil.createSafeSheetName(sheetName)
	    workbook.createSheet(renameSheetName)
	  }

	  /**
	   * Write a cell with given value. Columns must not clash.
	   */
	  def writeCell(value:Any, col:Short, cellRow:XSSFRow){
	    val cell = cellRow.createCell(col)
	    value match {
	      case str:String => cell.setCellValue(str)
	      case int:Int => cell.setCellValue(int)
	      case opt:Option[_] => cell.setCellValue(if(opt.isEmpty)"" else opt.get.toString)
	      case any => cell.setCellValue(any.toString)
	    }

	  }

	  /**
	   * Create a row
	   */
	  def createRow(sheet:XSSFSheet)(row:Int)={
	      sheet.createRow(row.toShort)
	  }

	  /**
	   * Write user detail
	   */
	  def writeUser(confirmed:Boolean, firstName:String, lastName:String, userModel:UserStorageModel, row:XSSFRow){
	    writeCell({if(confirmed) "Confirmed" else "Pending"} , 0, row)
	    writeCell(firstName , 1, row)
	    writeCell(lastName  , 2, row)

	    writeCell(userModel.email   , 3, row)
	    writeCell(userModel.ctcNo   , 4, row)

	    writeCell(userModel.addr  , 5, row)
	    writeCell(userModel.pstCd  , 6, row)
	    writeCell(userModel.state   , 7, row)

	  }

	  /**
	   * Create Header for each event
	   */
	  def createEventHeader(currRow:Int, sheet:XSSFSheet){
	    val row = createRow(sheet)(currRow)

	    for(cnt <- 0 until eventHeader.length){
		    writeCell(eventHeader(cnt), cnt.toShort, row)
	    }
	  }

	  /**
	   * Create Header for list of event
	   */
	  def createEventListHeader(currRow:Int, sheet:XSSFSheet){
	    val row = createRow(sheet)(currRow)

	    for(cnt <- 0 until eventListHeader.length){
		    writeCell(eventListHeader(cnt), cnt.toShort, row)
	    }
	  }

    /**
     * Create Corporate header
     */
    def createCorporateHeader(currRow:Int, sheet:XSSFSheet){
      val row = createRow(sheet)(currRow)

      for(cnt <- 0 until corporateHeader.length){
        writeCell(corporateHeader(cnt), cnt.toShort, row)
      }
    }

	  /**
	   * Write event
	   */
	  def writeEvent(currRow:Int, row:Int, event:CalendarRegisteredUser, sheet:XSSFSheet){
	    val rowCreator = createRow(sheet) _

	    //firstCol
	    val row1 = rowCreator(currRow)
	    writeCell("Title:", 0, row1)
	    writeCell(event.title,  1, row1)

	    //secondCol
	    val row2 = rowCreator(currRow+1)
	    writeCell("Description:", 0, row2)
	    writeCell(event.desc, 1, row2)

		//thirdCol
	    val row3 = rowCreator(currRow+2)
	      writeCell("Remaining Seats:", 0, row3)
		  writeCell(event.avail , 1, row3)

	    val row4 = rowCreator(currRow+3)
	    val row5 = rowCreator(currRow+4)
	    if(event.allDay){
		  writeCell("Date Time:", 0, row4)
		  writeCell(DATE_FORMAT.format(event.start), 1, row4)
	    }else{
	      writeCell("Start Date Time:", 0, row4)
		  writeCell(DATETIME_FORMAT.format(event.start), 1, row4)

		  writeCell("End Date Time:", 0, row5)
		  writeCell(DATETIME_FORMAT.format(event.end), 1, row5)
	    }
	  }

    /**
     * Write Corporate Info
     */
    def writeCorporateInfo(currRow:Int, sub:Subscription, sheet:XSSFSheet){
      val rowCreator = createRow(sheet) _

      //firstCol
      val row1 = rowCreator(currRow)
      writeCell(sub.cName,  0, row1)
      writeCell(sub.cDesc,  2, row1)
      writeCell(sub.cWebsite,  2, row1)
      writeCell(sub.cCtcNo,  3, row1)
      writeCell(sub.cEmail,  4, row1)


    }

	  /**
	   * Write event
	   */
	  def writeListEvent(currRow:Int, row:Int, event:CalendarRegisteredUser, sheet:XSSFSheet){
		  val rowCreator = createRow(sheet)(currRow + (row + 1))
		  writeCell(event.title , 0, rowCreator)
		  writeCell(event.desc  , 1, rowCreator)
		  if(event.allDay){
		     writeCell(DATE_FORMAT.format(event.start) , 2, rowCreator)
		  }else{
			 writeCell(DATETIME_FORMAT.format(event.start) , 2, rowCreator)
			 writeCell(DATETIME_FORMAT.format(event.end) , 3, rowCreator)
		  }
		  writeCell(event.avail, 4, rowCreator)
		  if(event.reg.isDefined)
			writeCell(event.reg.get.size, 5, rowCreator)
		  else
		    writeCell(0, 5, rowCreator)
	  }

	  /**
	   * Process all the events and every user.
	   */
	  def processEventList(eventList:List[CalendarRegisteredUser], workbook:XSSFWorkbook, mainSheet:XSSFSheet){
	    val futureList:List[Future[List[UserProfile]]] = Nil

      createEventListHeader(3, mainSheet)
	    val concurrList = for(eventCnt <- 0 until eventList.size) yield{
	      val event = eventList(eventCnt)
	      writeListEvent(3, eventCnt, event, mainSheet)
	      val listRegUserId = if(event.reg.isDefined && event.reg.get.size > 0 || event.pend.isDefined && event.pend.get.size > 0){
          val eventSheet = createSheet(workbook, event.title + "_" + DATE_FORMAT.format(event.start )+ "_" + eventCnt)
		      writeEvent(0, eventCnt, event, eventSheet)
			    createEventHeader(eventGap, eventSheet)
			    (processUserPerEvent(event.reg.getOrElse(Nil), event.pend.getOrElse(Nil), event, eventGap+1, eventSheet, workbook))
		      }else{
		        Future(Nil)
		      }
	      listRegUserId
      }

	    if(eventList.length > 0){
		    val toWait = Future.sequence(concurrList.toList)
		    Await.ready(toWait, 3.minutes)
		    closeExcel(workbook)
	    }else{
	    	val message = "Hello there,\n\n" +
	    		"There are no reservation created by you. So sorry.\n\n" +
	    		"Reminder: Beware of fraudelant emails. We from Walcron do not imply any charges from you for this service is provided free.\n\n"+
	    		"Sincerity from,\n"+
	    		"Walcron Coorperation";
	    	utils.Utility.sendEmail(Option.empty,
	        "Event Report",
	        message)
	    }
	  }

    def generateCoorporateInfo(): Future[List[Subscription]] ={
      val query = Json.obj(
        "_id" -> Json.obj("$oid" -> corporateId)
      )

      val cursor:Cursor[Subscription] = subscriptionCollection.find(query).cursor[Subscription]

      val futureSubList: Future[List[Subscription]] = cursor.collect[List]()
      futureSubList
    }

    def processCorporateInfo(sub: Subscription, mainSheet:XSSFSheet):Unit = {
      try {
        createCorporateHeader(0, mainSheet)
        writeCorporateInfo(1, sub, mainSheet)
      }catch{
        case e:Exception => e.printStackTrace();
      }
    }

    val cpInfoList = generateCoorporateInfo();
    cpInfoList.map { infoList =>
      infoList.size match{
        case 1 => {
          val workbook = prepareExcel()
          val mainSheet = createSheet(workbook, "Events")
          processCorporateInfo(infoList(0), mainSheet)
          val events = getListOfEvents()
          events.onComplete {
            case Success(eventList) => processEventList(eventList, workbook, mainSheet);
            case Failure(failure) => {
              removeFromDB();
              failure.printStackTrace();
              logActor ! ("Coorporate Id: " + corporateId + ", failure: " + failure.getMessage())
            }
          }
        }case _ =>{
          removeFromDB();
          Logger.info("There are no reports to be created for:"+corporateId);
        }
      }
    }
	}

	/**
	 * Actor Begins here.
	 **/
	def receive() = {
	  case ReportCreation(corporateId) =>{
	    if(checkSync(corporateId)){
		    val id = System.currentTimeMillis();
		    printSysData(id) // Start log
		    genExcel(corporateId)
		    sender ! new ReportCompletion(corporateId)
		    printSysData(id) // End log
	    }
	  }

	  case _ => {
	    logActor ! "Invalid report call"
	  }
	}

	def checkSync(corporateId:String):Boolean = {
	  import scala.concurrent.ExecutionContext.Implicits.global

	  val MAX_WAIT_DIFF = 5.minutes.toMillis

	  val query = Json.obj("_id"->corporateId)
	  val searchQuery:Cursor[ReportNotifyWithoutId] = reportCollection.find(query).cursor[ReportNotifyWithoutId]
	  val curFutureSubList: Future[List[ReportNotifyWithoutId]] = searchQuery.collect[List]()

	  val future = curFutureSubList.map(notify =>
		  notify.size match{
		    case 0 => {
		      val notifyObj = ReportNotify(corporateId, "N", System.currentTimeMillis())
		      reportCollection.insert(notifyObj)
		      true
		    }
		    case _ => {
		      if(notify(0).time < System.currentTimeMillis() - MAX_WAIT_DIFF){
		        reportCollection.remove(query, GetLastError(), false)
		        checkSync(corporateId)
		      }else{
		        false
		      }
		    }
		  }
		)

		val result = Await.result(future, 5.seconds)
		result
	}
}

case class ReportCreation(corporateId:String)
case class ReportCompletion(corporateId:String)
