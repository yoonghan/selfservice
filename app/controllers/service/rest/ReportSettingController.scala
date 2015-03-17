package controllers.service.rest

import play.api._
import play.api.mvc._
import controllers.service.BaseApiController
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import models.beans.ReportModel._
import play.api.Play.current
import play.api.libs.functional.syntax._
import scala.concurrent.Future
import reactivemongo.api._
import utils.CommonKeys._
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.annotations.ApiOperation
import com.wordnik.swagger.annotations.ApiResponses
import com.wordnik.swagger.annotations.ApiResponse
import scala.concurrent.duration._
import play.api.data.Forms._
import models.auth.SecurityKey
import models.beans.EnumTableList.REPORT_SETTING 
import controllers.jobs.ReportCreator

@Api(value = "/reportsetting", description = "Report Setting")
object ReportSettingController extends BaseApiController {
	def reminderCollection: JSONCollection = db.collection[JSONCollection](REPORT_SETTING.toString())
	
  /**
   * Insert or update reminder profile
   */
  @ApiOperation(
    nickname = "insertReportSetting", 
    value = "Creates or updates a report settings", 
    notes = "Creates or update a report settings", 
    response = classOf[String],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def settingIns = AuthorizeUser(BodyParsers.parse.json, AUTH_CAL_CREATE_LVL){request =>
    
    import reactivemongo.core.commands.GetLastError
    
    val _reportSetting = request.body.validate[ReportSetting];
    val userId = request.session(USER_ID)
    val oType = request.session(OTYPE)
    val cp_id = request.session(CP_ID)
    
    _reportSetting.fold(
        errors => {
          JsonResponse(BadRequest(Json.obj("error"->"Unexpected Request, what have you sent?")));
        },
        reportSetting => {
          val errorList = validateInput(reportSetting)
          if(errorList.isEmpty){
            
            val jsonObj = Json.toJson(reportSetting)
            
            val id = Json.obj("_id" -> cp_id)
            val modifier = Json.obj("modifier" -> userIDCombination(oType , userId))
            
            val upd_jsonObj = jsonObj.as[JsObject] ++ id ++ modifier
            
            reminderCollection.update(id, upd_jsonObj, GetLastError(), upsert = true, multi = false)
        	JsonResponse(Created(Json.obj("success"->"OK")))
          }else{
            JsonResponse(BadRequest(toReportSettingError(errorList)));
          }
        }
      )
  }
	
	
  /**
   * Retrieve report setting
   */
  @ApiOperation(
    nickname = "getReport", 
    value = "Report Setting", 
    notes = "Returns cp's report setting", 
    response = classOf[ReportSetting],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def setting = AuthorizeAsyncUser(BodyParsers.parse.anyContent, AUTH_CAL_CREATE_LVL){request =>    

    val cp_id = request.session(CP_ID)
	val query = Json.obj("_id" -> cp_id);

	val cursor:Cursor[ReportSetting] = reminderCollection.find(query).cursor[ReportSetting]
	val futureReportList: Future[List[ReportSetting]] = cursor.collect[List]()
	
    futureReportList.map { reportList =>
      reportList.size match {
        case 0 => JsonResponse(Ok(Json.obj("success" -> "ok")))
        case _ => JsonResponse(Ok(Json.toJson(reportList(0))))
      }
    }
  }
  
  /**
   * Retrieve report setting
   */
  @ApiOperation(
    nickname = "getReport", 
    value = "Check if allowed to view report",  
    response = classOf[Boolean],
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"),
		  			new ApiResponse(code = 404, message = "Record not found")))
  def isAllow = AuthorizeAsyncUser(BodyParsers.parse.anyContent, AUTH_CAL_CREATE_LVL){request =>    

    val cp_id = request.session(CP_ID)
	val query = Json.obj("_id" -> cp_id);

	val cursor:Cursor[ReportSetting] = reminderCollection.find(query).cursor[ReportSetting]
	val futureReportList: Future[List[ReportSetting]] = cursor.collect[List]()
	
    futureReportList.map { reportList =>
      reportList.size match {
        case 1 => JsonResponse(Ok(Json.obj("success" -> "ok")))
        case _ => JsonResponse(Ok(Json.obj("error" -> "Not found")))
      }
    }
  }
  
  /**
   * Generate report
   */
  @ApiOperation(
    nickname = "reportGeneration", 
    value = "Not Available", 
    notes = "Generate report based on user request", 
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access")))
  def genCorporateReport = AuthorizeUser(BodyParsers.parse.anyContent, AUTH_CAL_CREATE_LVL){ request =>
	import models.beans.SubscriptionModel._
    val cpId = request.session(CP_ID)
    
    ReportCreator.createCalendar(cpId)
    
    JsonResponse(Created(Json.obj("success"->"OK")))
  }
}